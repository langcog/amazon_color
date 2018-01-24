# Load term-sorting data --------------------------------------------------


spanish_terms <- c("Amarillo", "Azul", "Blanco", "Celeste", "Gris", "Marron", "Morado", "Naranja", "Negro", "Plomo", "Rojo", "Rosa", "Verde")

shipibo_terms <- c("Ambi", "Ami", "Barin Poi", "Bexnan", "Chexe", "Chimapo", "Emo", "Jimi", "Joshin", "Joxo", "Kari", "Kasho", "Keskiti", "Koin", "Kononbi", "Konron", "Koro", "Mai", "Mandi", "Manxan", "Maxe", "Nai", "Oxne", "Panshin", "Pasna", "Paxna", "Pei", "Pene", "Poa", "Ranchesh", "Tena", "Yame", "Yankon", "Wiso", "Xena", "Xexe", "Xo")

string_spelling_list <- "`Amarillo` = c('amarilla', 'amarillo'), `Ami` = c('ami'), `Ambi` = c('ambi'), `Azul` = c('azul', 'azu'), `Barin Poi` = c('barin pui', 'barrin pui', 'barrinpui', 'pui', 'barin poi', 'barrin poi', 'bavrinpui*', 'barri'), `Bexnan` = c('berrnan', 'bexna', 'bexnan'), `Blanco` = c('blanco'), `Celeste` = c('celeste'), `Chexe` = c('chese', 'chexe'), `Chimapo` = c('chimapu'), `Emo` = c('emu'), `Gris` = c('gris'), `Jimi` = c('jimi'), `Jimi Manxan` = c('jimi manxan'), `Jisa` = c('jisa'), `Joa` = c('joa'), `Joshin` = c('joshin', 'joxin', 'toshin'), `Joxo` = c('josho', 'joxo'), `Kari` = c('cari', 'carri', 'kari', 'karri'), `Kasho` = c('kashos'), `Keskiti` = c('kex keti'), `Koin` = c('kuin'), `Kononbi` = c('kunumbi'), `Konron` = c('korrum', 'kumrrum', 'kunrrum'), `Koro` = c('coro'), `Mai` = c('mai'), `Mandi` = c('mandi'), `Manxan` = c('manrran', 'manshan', 'manxam', 'manxan', 'maxan', 'maxna'), `Manxan Yankon` = c('manxan yankon'), `Marron` = c('marron'), `Maxe` = c('maxe'), `Mierda Sol` = c('miarda', 'miarda del sol'), `Morado` = c('bioleta', 'morado', 'violeta', 'morada'), `Nai` = c('nai', 'nia'), `Naranja` = c('naranja', 'naranjada', 'narranxa', 'naranjado', 'narango', 'naranjo', 'anaranjado'), `Negro` = c('negro'), `Oxne` = c('oshne', 'oxne', 'oxe'), `Pei` = c('pei'), `Poa` = c('pua'), `Pene` = c('pene'), `Panshin` = c('panshin'), `Pasna` = c('paxsna', 'pasna'), `Paxna` = c('parrna', 'paxna'), `Plomo` = c('plomo'), `Ranchesh` = c('ranchex'), `Rojo` = c('rojo', 'roja'), `Rosa` = c('rosada', 'rosa', 'rosado'), `Spanish Term` = c('carne', 'agua', 'agur', 'uva color*', 'pasto payota', 'chocolate', 'coral', 'oscuro', 'lila', 'color cielo', 'cielo'), `Tena` = c('tena'), `Verde` = c('verde', 'cerde', 'verdesito'), `Wiso` = c('wiso'), `Xena` = c('xena'), `Xo` = c('xo'), `Xexe` = c('xexe', 'xexi'), `Yame` = c('rayame', 'yame'), `Yame Wiso` = c('yame wiso'), `Yankon` = c('rayanko', 'yankom', 'yankon', 'yankum', 'yankun', 'yankontani', 'yakon', 'yakun', 'yankoncha'), `NA` = c(NA)"

spelling_list <- eval(parse(text = paste0("c(",string_spelling_list,")")))


# Load in data from current study -----------------------------------------


naming_data <- read_csv("data/Current_Data/naming_colors_participants.csv") %>%
  left_join(read_csv("data/Current_Data/naming_colors_data.csv"), by = 'subj') %>%
  mutate(color_cat = ifelse(is.na(color_cat), first_response, color_cat)) %>%
  mutate(color_cat = ifelse(color_cat %in% unlist(spelling_list), color_cat, NA)) %>%
  mutate(color_cat = eval( parse(text = gsub(pattern = "x", replacement = string_spelling_list, "forcats::fct_collapse(color_cat, x)")))
  )

grouping_data <- read_csv("data/Current_Data/grouping_colors_participants.csv") %>%
  left_join(read_csv("data/Current_Data/grouping_colors_data.csv"), by = 'subj') %>%
  mutate(`nombre del grupo` = ifelse(`nombre del grupo` %in% unlist(spelling_list), 
                                     `nombre del grupo`, NA)) %>%
  mutate(`nombre del grupo` = eval( parse(text = gsub(pattern = "x", replacement = string_spelling_list, "forcats::fct_collapse(`nombre del grupo`, x)")))
  )


# Load in Kay data --------------------------------------------------------


kay_langs <- read_tsv("data/WCS_Data/lang.txt", 
                      col_names = c('WCS Language Number', 'WCS Language Name', 
                                    'WCS Language Geographic Location', 'Field Worker')) %>%
  filter(`WCS Language Name` == 'Shipibo')

kay_dict <- read_tsv("data/WCS_Data/dict.txt",
                     col_names = c('WCS Language Number', 'Term Number',
                                   'Term', 'Term Abbreviation')) %>%
  filter(`WCS Language Number` == kay_langs$`WCS Language Number`)

kay_foci <- read_tsv("data/WCS_Data/foci-exp.txt",
                     col_names = c("WCS Language Number", "WCS Speaker Number",
                                   "WCS Focus Response", "Term Abbreviation",
                                   "Single Chip")) %>%
  filter(`WCS Language Number` == kay_langs$`WCS Language Number`)

kay_speaker <- read_tsv("data/WCS_Data/spkr-lsas.txt", 
                        col_names = c("WCS Language Number", "WCS Speaker Number",
                                      "WCS Speaker Age", "WCS Speaker Sex")) %>%
  filter(`WCS Language Number` == kay_langs$`WCS Language Number`)

kay_terms <- read_tsv("data/WCS_Data/term.txt", 
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
                                      `Pasna` = c("pashnatani"),
                                      `Pene` = c("pene"),
                                      `Pota'` = c("pota'"),
                                      `Xena` = c("shane"),
                                      `Xo` = c("shoo"),
                                      `Yame` = c("yametani"),
                                      `Yankon` = c("yancon"),
                                      `Wiso` = c("huiso")
  ))


# Load chip set and terms data --------------------------------------------


color_chip_data <- read_csv("data/wcs_measures.csv", skip = 1) %>%
  mutate(V = factor(V, levels = LETTERS[seq(n_distinct(V),1)])) %>%
  mutate(hex = colorspace::hex(
    colorspace::LAB(.data$`L*`, .data$`a*`, 
                    .data$`b*`, .data$`#cnum`), fixup = T))

shipibo_chip_set <- read.csv(text = "shipibo, spanish, munsell_code, chip_id
                             Joshin, rojo, G3, 245
                             Pei/Xo, verde, G18, 234
                             Panshin, amarillo, C9, 297
                             Wiso, negro, J1/I0, 312
                             Joxo, blanco, A/B0, 274
                             Nai, celeste, E29, 1
                             Ami/Poa, morado, H36, 325
                             Barin poi, mierda sol, F12, 320")

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
                             Rosa, RS, F39, 65")


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


