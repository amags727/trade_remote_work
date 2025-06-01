from cleanco import countrysources, matches, basename
from cleanco.clean import custom_basename, terms_by_type, terms_by_country, normalize_terms, normalized, strip_tail
import re; import unicodedata
import functools; import operator
import time 
import html

def pre_cleaning(input_string):
    input_string = html.unescape(input_string)
    input_string = input_string.replace("™", "")
    input_string = input_string.lower() # convert to lower 
    input_string = unicodedata.normalize('NFKD', input_string).encode('ASCII', 'ignore').decode()
    input_string = re.sub(r'\b(et|and)\b', '', input_string)  # delete et / and 
    input_string = re.sub(r"^l'", "", input_string) # remove l' if it starts the string 
    input_string = re.sub(r"[&'+-]", ' ', input_string) # for places where we have a spacer character put a space 
    split_strings = input_string.split()   # Split the string into words
    split_strings = [re.sub(r"[^a-zA-Z0-9]", '', word) for word in split_strings] # Remove non-alphanumeric characters from each word
    input_string = " ".join(split_strings)  # Join the cleaned words back into a single string
    input_string = re.sub(r'\b([a-z])(?:\s+([a-z]))+\b', lambda m: ''.join(m.group(0).split()), input_string) #collapse single letters spaced apart into a whole 

    return(input_string)

def clean_firm_names(df, name_column, classify_nationality):
   # Additional French firm types commented terms already in the dataset
    additional_french_firm_types = ["Profession libérale",
                                    "Communauté d Agglomération","COMMUNAUTE D AGGLOMERATION", "ca", 
                                    "Sociétés d'exercice libéral", "sel", 
                                    "Société d Exercice Libéral par Actions Simplifié", "SELAS",
                                    "Société d'investissement à capital fixe", "sicaf", 
                                    "association de fait",  "association non déclarée",
                                    "Société en participation", "SEP", 
                                    "Société d Exercice Libéral à Responsabilité Limitée", "selarl",
                                    "Société en participation de professions libérales", "SPPL",
                                    "Société en participation avec personne morale", "SEP-PM",
                                    "Société en participation entre personnes physiques", "SEP-PP",
                                    "Société d'investissement à capital variable",
                                   
                                    ### sarl variants 
                                    'Exploitation Agricole à Responsabilité Limitée ', "EARL",
                                    'societe a responsabilite limite',
                                    "Société à responsabilité limitée", 
                                    "Société à responsabilité limitée à associé unique",
                                    "SARL à associé unique", 
                                    "SARL unipersonnelle", 
                                    'société à responsabilité limitée unipersonnelle',
                                    ##SA variants 
                                    'Société anonyme', 'sa', 
                                    'Société Anonyme coopérative de banque populaire',
                                    'Société Anonyme coopérative à capital variable',
                                    'Société anonyme coopérative', 
                                    'SA COOPERATIVE', 
                                    'Société Anonyme à Directoire',
                                    'Société Anonyme à Conseil d Administration', 
                                    'Société par actions simplifié', 
                                    'SA à conseil d administration',
                                    'Société Anonyme d Economie Mixte'
                                    #non profit
                                    'association Loi de 1901 reconnue comme organisation interprofessionnelle',
                                    'association Loi de 1901',
                                    'Loi 1901',
                                    'Association loi 1901',
                                    'association de loi 1901',
                                    'Association sans but lucratif',
                                    'ASBL',
                                    
                                    ## SAS
                                    'Société par actions simplifiée à associé unique',
                                    'societe par action simplifiee a associe unique',
                                    "Société par actions simplifiée", 
                                    "societe par actions simplifiees",
                                    'societe par actions simplfiee', 
                                    "societe par action simplifiee",
                                    "Société par Actions Simplifiée Unipersonnelle",
                        
                                    # Other 
                                    'Société Civile de Construction Vente', 'SCCV',
                                    'Coopérative d’Utilisation de Matériel Agricole', 'CUMA',
                                    'Groupement Agricole d’Exploitation en Commun', 'GAEC',
                                    'Société Civile de Fait', 'SCDF',
                                    'Groupement Foncier Agricole', 'GFA',
                                    'Organisme de Gestion de l’Enseignement Catholique', 'OGEC',
                                    'COLLECTIVITE Territoriale',
                                    'Syndicat Mixte',
                                    'Syndicat mixte communal',
                                    'Société Coopérative Agricole',
                                    'Groupement Foncier Agricole',
                                    'SOCIETE ECONOMIE MIXTE ',
                                    'Société d Économie Mixte Locale',
                                    'SEML', 'SCEA', 'SASP','SCV',
                                    'Société Civile d Exploitation Agricole',
                                    'Société Anonyme Sportive Professionnelle',
                                    'Société Coopérative Agricole',
                                    'dont le siège social',
                                    'SOCIETE COOPERATIVE VINICOLE',
                                    'Société Civile Cooperative',
                                    'société de fait',
                                    'Etablissement public de coopération intercommunale',
                                    'Union de syndicats professionnels',
                                    "Entreprise individuelle", "entreprise en nom personnel", 
                                    "Entreprise unipersonnelle à responsabilité limitée",  #EURL
                                    "Fonds commun de placement",  #FCP
                                    'groupement d interet economique', #GIE
                                    "societe cooperative artisanale a forme anonyme", 
                                    "Société en participation",  #SEP
                                    "Société en nom collectif", #SNC
                                    "Société en commandite simple", #SCS
                                    "Société en commandite par actions", #SCA
                                    "Société coopérative de production", #SCOP
                                    "Société d économie mixte", #sem
                                    "Association Déclarée", 'ad',
                                    "Société Civile de Moyens", 'scm',
                                    "Société Civile Immobilière", 'sci',
                                    "Société Civile",'sc', 
                                    "Société Civile Professionnelle",'scp',]


    # Define helper function to check if the firm is French
    def check_if_french(name):
        name = strip_tail(name)
        
        if not name.strip():
            return "", "unknown"
        parts = [name.split()[0], name.split()[-1]]  # Extract first and last words
        nparts = [normalized(p) for p in parts]  # Normalize the parts
        matches = []
        extracted = []

        for classifier, term in countries:
            nterm = normalized(term)  # Normalize term for comparison
            try:
                idx = nparts.index(nterm)  # Check if term matches any part
                extracted.append(term)
            except ValueError:
                pass
            else:
                matches.append(classifier)
        
        # Remove duplicates from extracted terms
        extracted = " ".join(dict.fromkeys(extracted))
        
        # Determine the classification
        if matches and "France" not in matches:
            return extracted, 'unlikely french'
        elif matches and "France" in matches:
            return extracted, 'likely french'
        else:
            return extracted, 'unknown'

    
    # Prepare the input for the matching function
    ts = functools.reduce(operator.iconcat, terms_by_type.values(), [])
    cs = functools.reduce(operator.iconcat, terms_by_country.values(), [])
    raw_terms = set(ts + cs + additional_french_firm_types)
    nterms = normalize_terms(raw_terms)
    ntermparts = (t.split() for t in nterms)
    sntermparts = sorted(ntermparts, key=lambda x: (-len(x), x))
    term_output = [(len(tp), tp) for tp in sntermparts]

    # Setup the input for country assignment
    countries = []
    for country in terms_by_country:
        for item in terms_by_country[country]:
            countries.append((country, item))
    countries += [('France', term) for term in additional_french_firm_types]
    countries = sorted(countries, key=lambda part: len(part[1]), reverse=True)

    
    # Apply the functions to the 'names' series
    names = df[name_column].apply(pre_cleaning)
    cleaned_names = names.apply(lambda x: custom_basename(x, term_output, suffix=True, prefix=True, middle=True))

   
    df = df.assign(**{f"{name_column}_cleaned": cleaned_names})

    if classify_nationality:
        french_checks = names.apply(check_if_french)
        extracted_terms = french_checks.apply(lambda x: x[0])  # Extracted terms
        french_likelihood = french_checks.apply(lambda x: x[1])  # Likelihood (likely/unlikely/unknown)

        df = df.assign(**{
            "extracted_terms": extracted_terms,
            "firm_type_french_likelihood": french_likelihood
        })
    return df



def strip_words(df, name_column, common_words):
    def stripping(text, common_words):
        if not isinstance(text, str):
            return text  # Handle non-string inputs gracefully
        # Create a regex pattern for full-word matches of common words
        pattern = r'\b(' + '|'.join(map(re.escape, common_words)) + r')\b'
        # Replace matched words with an empty string
        cleaned_text = re.sub(pattern, '', text)
        # Remove extra spaces that may result
        return " ".join(cleaned_text.split())
    
    stripped_names = df[name_column].apply(lambda x: stripping(x, common_words))
    df = df.assign(**{f"{name_column.replace('cleaned','stripped')}": stripped_names})
    return df
def tic():
    global start_time
    start_time = time.time()

def toc():
    elapsed_time = time.time() - start_time
    print(f"Elapsed time: {elapsed_time:.2f} seconds")
    return elapsed_time