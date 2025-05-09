

# import the raw results  -------------------------------------------------
full_output = import_file("3) output/0_raw_output/03_25_raw_output.rds")
for (name in names(full_output)) assign(name, full_output[[name]])



# output block 2 variance results  ---------------------------------------------------------

model_inputs = model_output[variations[controls != "" & grepl('mean|max_quartile',ind_var)
                & dep_var %in% gpaste('de_trended_variance_log_',c('value_customs_export', 'dom_turnover'))] %>% 
       .[order(dep_var,grepl('quartile', ind_var), grepl('share', ind_var))] %>% pull(counter)]

label = '03_25_block_2'
format_table(model_inputs,
             label = label,
             coef_names = c('data intensity', 'log age', 'log years observed',
                            'data intensity', gpaste('quartile ', c(4,2:3,4,3,2)), 
                            'log years exported'),
             coef_order = c(1,5,6,4,2:3,7),
             custom_rows = list(c("data intensity quartile", rep("",8)),
                                c('data intensity var', rep(c('log', 'share'),4))),
             custom_row_placement = c(10,24),
             divisions_before = 5,
             headers = gpaste("&",gpaste('\\multicolumn{4}{c}{',
                                         c('Domestic', 'Export'), " Revenue Variance}",
                                         collapse_str = "& &"),'\\\\'),
             notes = paste0('Robust standard errors clustered at the industry level.',
                            'All regressions include industry FE'),
             note_width = 1.2,
             rescale_factor = 1,
             output_path = paste0('3) output/', label, '.tex'))
# output block 10, entry / market exit ------------------------------------
model_inputs = model_output[variations[block == 10 & !grepl('\\*', controls) & controls !=""] %>%
                            .[order(dep_var,grepl('quartile', ind_var), grepl('share', ind_var))] %>% pull(counter)]
label = '03_25_block_10'


format_table(model_inputs,
             label = label,
             coef_names = c('data intensity', 'log dom. revenue', 'data intensity',
                            rep(gpaste('\\multicolumn{1}{r}{quartile ',2:4, "}"),2),
                            'log other export\nmarkets'),
             coef_order = c(1,3:5, 2,6),
             custom_rows = list(c("data intensity quartile", rep("",8)),
                                c('data intensity var', rep(c('log', 'share'),4))),
             custom_row_placement = c(10,22),
             divisions_before = 5,
             headers = gpaste("&",gpaste('\\multicolumn{4}{c}{',
                                         c('Begin Exporting', 'Leave Export Market'), "}",
                                         collapse_str = "& &"),'\\\\'),
             notes = paste0('Robust standard errors clustered at the firm-level.',
                       ' Time to export regressions include industry and year FE.',
                       ' Market exit regressions include industry, year, country FE.',
                       ' Quartiles express ranking within a given industry-year group.'),
             note_width = 1.2,
             cox = T,
             rescale_factor = 1,
             output_path = paste0('3) output/', label, '.tex'))

