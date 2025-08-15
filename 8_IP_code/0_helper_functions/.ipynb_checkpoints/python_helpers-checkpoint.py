import html; import re; import pandas as pd
def clean_firm_name(string):
    replacements = [('à|â|ä','a'), ('é|è|ê|ë','e'), ('ç', 'c'), ('i|î|ï', 'i'), 
                    ('ô|ö', 'o'), ('ù|û|ü', 'u'), ('ñ', 'n'), ('ÿ', 'y'), ("\.", "")]             
    string = html.unescape(string)
    string = string.lower()
    for old, new in replacements:
        string = re.sub(old, new, string)  
    string = string.strip()
    return string

def remove_ancillary(string):
   return re.sub("\)|,", "", string).strip()  
def extract_and_count(strings, pattern):
    # List to store all substrings inside parentheses
    all_matches = []
    for string in strings:
        # Find all substrings inside parentheses
        matches = re.findall(pattern, string)
        all_matches.extend(matches)
    
    # Create a DataFrame with counts
    counts = pd.DataFrame(all_matches, columns=['Substring'])
    counts = counts.value_counts().reset_index(name='Count')
    return counts
