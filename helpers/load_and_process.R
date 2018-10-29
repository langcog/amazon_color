color_sheet <- readxl::read_excel("../data/shipibo_colors (2018_02_21).xlsx") %>%
  filter(!is.na(`Term (2017 survey)`))

filter(color_sheet, language == "SK" & nature == "BCT")$`Term (2017 survey)`



# Load term-sorting data --------------------------------------------------

shipibo_bct <- filter(color_sheet, language == "SK" & grepl("\\bBCT\\b", nature))$`Term (2017 survey)`

shipibo_nbct <- filter(color_sheet, language == "SK" & grepl("\\bNBCT\\b", nature))$`Term (2017 survey)`

shipibo_ahct <-  filter(color_sheet, language == "SK" & grepl("\\bAHCT\\b", nature))$`Term (2017 survey)`

shipibo_slt <-  filter(color_sheet, language == "SK" & grepl("\\bSLT\\b", nature))$`Term (2017 survey)`

shipibo_mt <-  filter(color_sheet, language == "SK" & grepl("\\bMT\\b", nature))$`Term (2017 survey)`

shipibo_ot <-  filter(color_sheet, language == "SK" & grepl("\\bOT\\b", nature))$`Term (2017 survey)`

shipibo_terms <- filter(color_sheet, language == "SK")$`Term (2017 survey)`


spanish_bct <- filter(color_sheet, language == "SP" & grepl("\\bBCT\\b", nature))$`Term (2017 survey)`

spanish_nbct <- filter(color_sheet, language == "SP" & grepl("\\bNBCT\\b", nature))$`Term (2017 survey)`

spanish_ahct <-  filter(color_sheet, language == "SP" & grepl("\\bAHCT\\b", nature))$`Term (2017 survey)`

spanish_slt <-  filter(color_sheet, language == "SP" & grepl("\\bSLT\\b", nature))$`Term (2017 survey)`

spanish_mt <-  filter(color_sheet, language == "SP" & grepl("\\bMT\\b", nature))$`Term (2017 survey)`

spanish_ot <-  filter(color_sheet, language == "SP" & grepl("\\bOT\\b", nature))$`Term (2017 survey)`

spanish_terms <- filter(color_sheet, language == "SP")$`Term (2017 survey)`

color_sheet$`Alternate spellings (2017 survey-adults)`

spelling_list <- setNames(str_split(apply(color_sheet[, c("Term (2017 survey)", "Alternate spellings (2017 survey-adults)", "Alternate spellings (2017 survey-children)", "Term (WCS)")], 1, function(x) tolower(toString(na.omit(x)))), ",(\\s)?"), color_sheet$`Term (2017 survey)`)


# Load chip set and terms data --------------------------------------------


color_chip_data <- read_csv("../data/wcs_measures.csv", skip = 1) %>%
  mutate(V = factor(V, levels = LETTERS[seq(n_distinct(V),1)])) %>%
  mutate(hex = colorspace::hex(
    colorspace::LAB(.data$`L*`, .data$`a*`, 
                    .data$`b*`, .data$`#cnum`), fixup = T))

shipibo_chip_set <- read.csv(text = "shipibo, spanish, munsell_code, chip_id
                             Joshin, Rojo, G3, 245
                             Pei/Xo, Verde, G18, 234
                             Panshin, Amarillo, C9, 297
                             Wiso, Negro, J1/I0, 312
                             Joxo, Blanco, A/B0, 274
                             Nai, Celeste, E29, 1
                             Ami/Poa, Morado, H36, 325
                             Barin Poi, Mierda sol, F12, 320",
                             stringsAsFactors = FALSE) %>%
  mutate_if(is.character, trimws)

spanish_chip_set <- read.csv(text = "spanish, code, munsell_code, chip_id
                             Blanco, BL, A/B0, 274
                             Verde, VD, G18, 234
                             Rojo, RJ, G3, 245
                             Amarillo, AM, C9, 297
                             Azul, AZ, F30, 291
                             Negro, NG, J1/I0, 312
                             Naranja, NR, E4, 121
                             Gris, GR, F0, 46
                             Morado, MRD, H36, 325
                             Marron, MRN, G5, 266
                             Rosa, RS, F39, 65",
                             stringsAsFactors = FALSE) %>%
  mutate_if(is.character, trimws)



# Load in data from current study -----------------------------------------


naming_data <- read_csv("../data/Current_Data/naming_colors_participants.csv") %>%
  left_join(read_csv("../data/Current_Data/naming_colors_data.csv"), by = 'subj') %>%
  mutate(color_cat = ifelse(is.na(color_cat), first_response, color_cat)) %>%
  mutate(color_cat = do.call(forcats::fct_collapse, list(color_cat, !!!spelling_list)))


grouping_data <- read_csv("../data/Current_Data/grouping_colors_participants.csv") %>%
  left_join(read_csv("../data/Current_Data/grouping_colors_data.csv"), by = 'subj') %>%
  mutate(`nombre del grupo` = ifelse(`nombre del grupo` %in% unlist(spelling_list), 
                                     `nombre del grupo`, NA)) %>%
  mutate(`nombre del grupo` = do.call(forcats::fct_collapse, list(`nombre del grupo`, !!!spelling_list)))
  


# Load in Kay data --------------------------------------------------------


kay_langs <- read_tsv("../data/WCS_Data/lang.txt", 
                      col_names = c('WCS Language Number', 'WCS Language Name', 
                                    'WCS Language Geographic Location', 'Field Worker')) %>%
  filter(`WCS Language Name` == 'Shipibo')

kay_dict <- read_tsv("../data/WCS_Data/dict.txt",
                     col_names = c('WCS Language Number', 'Term Number',
                                   'Term', 'Term Abbreviation')) %>%
  filter(`WCS Language Number` == kay_langs$`WCS Language Number`)

kay_foci <- read_tsv("../data/WCS_Data/foci-exp.txt",
                     col_names = c("WCS Language Number", "WCS Speaker Number",
                                   "WCS Focus Response", "Term Abbreviation",
                                   "Single Chip")) %>%
  filter(`WCS Language Number` == kay_langs$`WCS Language Number`)

kay_speaker <- read_tsv("../data/WCS_Data/spkr-lsas.txt", 
                        col_names = c("WCS Language Number", "WCS Speaker Number",
                                      "WCS Speaker Age", "WCS Speaker Sex")) %>%
  filter(`WCS Language Number` == kay_langs$`WCS Language Number`)

kay_terms <- read_tsv("../data/WCS_Data/term.txt", 
                      col_names = c('WCS Language Number', 'WCS Speaker Number', 
                                    'WCS Chip Number', 'Term Abbreviation')) %>%
  filter(`WCS Language Number` == kay_langs$`WCS Language Number`) %>%
  left_join(select(kay_dict, Term, `Term Abbreviation`), by = "Term Abbreviation") %>%
  left_join(select(kay_speaker, -`WCS Language Number`), by = "WCS Speaker Number") %>%
  mutate(Term = forcats::fct_collapse(Term,
                                      `Ami` = c("ami"),
                                      `Barin Poi` = c("barin poi"),
                                      `Cana` = c("cana"),
                                      `Chexe` = c("cheshe"),
                                      `Chimapo` = c("chimapo"),
                                      `Koro` = c("coro"),
                                      `Emo` = c("emo"),
                                      `Joshin` = c("joshin"),
                                      `Joxo` = c("josho"),
                                      `Kaqui` = c("kaqui"),
                                      `Mai` = c("mai"),
                                      `Manxan` = c("manshan"),
                                      `Panshin` = c("panshin"),
                                      `Paxna` = c("pashnatani"),
                                      `Pene` = c("pene"),
                                      `Pota'` = c("pota'"),
                                      `Xena` = c("shane"),
                                      `Xo` = c("shoo"),
                                      `Yame` = c("yametani"),
                                      `Yankon` = c("yancon"),
                                      `Wiso` = c("huiso")
  ))


# Load assigned colors for terms ------------------------------------------


graph_colors <- c(
  'Amarillo' = '#FFD416',
  'Ambi' = '#874A8C',
  'Ami' = '#76296E',
  'Azul' = '#337DCE',
  'Barin Poi' = '#6D6212',
  'Bexnan' = '#B6D744',
  'Blanco' = '#DBDBDB',
  'Celeste' = '#74DFF7', 
  'Chexe' = '#81C147',
  'Chimapo' = '#003459',
  'Emo' = '#007177',
  'Gris' = '#979997', 
  'Jimi' = '#822158',
  'Joshin' = '#BC1E47',
  'Joxo' = '#DFE6F0',
  'Kari' = '#571848',
  'Kasho' = '#F07000',
  'Keskiti' = '#E56F92',
  'Koin' = '#50491D',
  'Kononbi' = '#503B87',
  'Konron' = '#BB8F00',
  'Koro' = '#7B7B7B',
  'Mai' = '#7F5A21',
  'Mandi' = '#005637',
  'Manxan' = '#FEBBA1',
  'Marron' = '#9E5E22', 
  'Maxe' = '#DC4800',
  'Morado' = '#B175F9', 
  'Nai' = '#19A2C2',
  'Naranja' = '#FF6E00',
  'Negro' = '#000000',
  'Oxne' = '#66BCC9',
  'Panshin' = '#EDC800',
  'Pasna' = '#D3C5DF',
  'Paxna' = '#EC99A2',
  'Pei' = '#69C360',
  'Pene' = '#55471E',
  'Plomo' = '#848484', 
  'Poa' = '#7E4E94',
  'Ranchesh' = '#4A2347',
  'Rojo' = '#E03D28', 
  'Rosa' = '#FF8C9D', 
  'Tena' = '#C5D500',
  'Verde' = '#61E27B', 
  'Yame' = '#666412',
  'Yankon' = '#00A79E',
  'Wiso' = '#272727',
  'Xena' = '#D4799C',
  'Xexe' = '#9769AE',
  'Xo' = '#3A6E14',
  'Spanish Term' = '#FF6E00'
)


