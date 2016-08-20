###################################################################################
# Kaggle What is Cooking Competition Project
#
# 1. Environment Setup
# 2. Load Training and Test data sets
# 3. Data Cleaning and Exploration
# 4. Create Validation data set from Training data set
# 5. Build ML Models 
# 6. Predict cuisine for the Validation data set and check accuracy
# 7. Build ML Models with complete Training data set
# 8. Predict cuisine for the complete Test data set
# 9. Create Submission file
##################################################################################


######################### ENIRONMENT SETUP - BEGIN ###############################   

# Clear all variables and devices (used for plotting) in the environment
rm(list=ls())
dev.off()

# Load the required packages (if packages are not available, install them first)
for (package in c('jsonlite', 'ggplot2', 'caret', 'tm', 
                  'wordcloud', 'plyr', 'data.table', 'party',
                  'xgboost', 'Matrix', 'stm', 'topicmodels',
                  'randomForest', 'e1071', 'h2o')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Declare all local variables
file.path <- "C://Users//Linesh//Documents//Linesh//General//DS Meetup//Kaggle//Cooking//Input Files//"
output.path <- "C://Users//Linesh//Documents//Linesh//General//DS Meetup//Kaggle//Cooking//Output Files//"
saved.obj <- "C://Users//Linesh//Documents//Linesh//General//DS Meetup//Kaggle//Cooking//R-Objects//"

######################### ENIRONMENT SETUP END #################################   


######################### LOAD DATASET - BEGIN #################################   

# Load train and test data sets
test.dat <- fromJSON(paste0(file.path, "test.json"))
train.dat <- fromJSON(paste0(file.path, "train.json"))

dim(train.dat)
# [1] 39774     3

dim(test.dat)
# [1] 9944    2

######################### LOAD DATASET - END ###################################   


######################### DATA CLEANING AND EXPLORATION - BEGIN ################   

# combine both the train and test data sets - useful for clean-up process
combined.data <- rbind(train.dat[,c("id", "ingredients")],test.dat)  

combined.data$ingredients <- lapply(combined.data$ingredients, 
                                    FUN=function(x) gsub("-", " ", x))  

combined.data$ingredients <- lapply(combined.data$ingredients, 
                                    FUN=function(x) gsub("7 up", "sevenup", x))  

combined.data$ingredients <- lapply(combined.data$ingredients, 
                                    FUN=function(x) gsub("sun dried", "sundried", x))  

combined.data$ingredients <- lapply(combined.data$ingredients, 
                                    FUN=function(x) gsub("bone less", "boneless", x))  

combined.data$ingredients <- lapply(combined.data$ingredients, 
                                    FUN=function(x) gsub("skin less", "skinless", x))  

combined.data$ingredients <- lapply(combined.data$ingredients, 
                                    FUN=function(x) gsub("bay leaf", "bayleaf", x)) 

replacement.phrase <- list(c("green onion", "red onion", "purple onion",
                             "yellow onion", "yel onion"),
                           c("collard green leaves", "collards", "collard leaves"),
                           c("black pepper", "ground pepper"),
                           c("yel chives"),
                           c("spinach leaves"),
                           c("tea leaves"),
                           c("chile"),
                           c("garlic clove", "garlic bulb"),
                           c("uncooked"),
                           c("red chili pepper", "hot chili pepper", "red hot chili pepper"),
                           c("baking potato", "baked potato"),
                           c("table salt", "white salt"),
                           c("i cant believe its not butter spread", "i cant believe its not butter"),
                           c("extra virgin olive oil", "virgin olive oil"),
                           c("confectioners sugar"),
                           c("asafetida"),
                           c("channa"),
                           c("T", "'", "®"),
                           c("daal"),
                           c("yoghurt"))

replacement.word <- list("onion", "collared green", "pepper", "chives", "spinach", "tea",
                         "chili", "garlic", "raw", "chili pepper", "baked potato", "salt",
                         "butter", "olive oil", "powdered sugar", "asafoetida", "chana", "",
                         "dal", "yogurt")

for (i in 1:length(replacement.phrase)) {
  for (vec in replacement.phrase[i]) {
    for (phr in vec) {
      combined.data$ingredients <- lapply(combined.data$ingredients, 
                                          FUN=function(x) gsub(phr, replacement.word[i], x))  
      print(vec)
      print(phr)
      print(replacement.word[i])
    }
  }
}

head(combined.data$ingredients, 2)
# [[1]]
# [1] "romaine lettuce"      "black olives"         "grape tomatoes"       "garlic"              
# [5] "pepper"               "onion"                "seasoning"            "garbanzo beans"      
# [9] "feta cheese crumbles"
# 
# [[2]]
# [1] "plain flour"      "pepper"           "salt"             "tomatoes"        
# [5] "pepper"           "thyme"            "eggs"             "green tomatoes"  
# [9] "yellow corn meal" "milk"             "vegetable oil"   

# Create a corpus of ingredient terms
corpus.ingredients <- Corpus(VectorSource(combined.data$ingredients)) 
# corpus ingredients has 49,718 elements, and its size is 209.8 MB

save(combined.data, file=paste0(saved.obj, "combined-data-new-R-Object"))
rm(combined.data)

# Remove the numbers
corpus.cleaned <- tm_map(corpus.ingredients, removeNumbers) 
# corpus.cleaned has same number of elements, and its size remans 209.8 MB
corpus.cleaned[[1]]
# <<PlainTextDocument (metadata: 7)>>
#   romaine lettuce
# black olives
# grape tomatoes
# garlic
# pepper
# onion
# seasoning
# garbanzo beans
# feta cheese crumbles

save(corpus.ingredients, file=paste0(saved.obj, "corpus-ingredients-new-R-Object"))
rm(corpus.ingredients)

# Remove the punctuations
corpus.cleaned <- tm_map(corpus.cleaned, removePunctuation) 
# corpus.cleaned has same number of elements and size too
corpus.cleaned[[15]]
# <<PlainTextDocument (metadata: 7)>>
#   fresh parmesan cheese
# butter
# all purpose flour
# fat free less sodium chicken broth
# chopped fresh chives
# gruyere cheese
# pepper
# bacon slices
# gnocchi
# fat free milk
# cooking spray
# salt

brandnames = c("alexia", "breakstones", "kraft", "bertolli classico", "bertolli", 
               "best foods", "betty crocker", "bisquick", "bob evans", "breyers", 
               "curry guy", "camellia", "campbells", "country crock", "crisco",
               "crystal farms", "delallo", "diamond crystal", "domino", 
               "doritos", "earth balance", "egglands best", "foster farms", 
               "franks", "gold medal", "goya", "green giant steamers niblets", 
               "green giant", "heinz", "hellmanns", "herdez", 
               "hidden valley", "honeysuckle white", "jacksonville", "jimmy dean",
               "johnsonville", "knorr", "krudsen", "kikkoman", "lipton", "land o lakes",
               "mazola", "lea and perrins", "mccormick", "meyer", "mission", "old el paso",
               "old bay", "pam", "pepperidge farm", "oscar mayer", "pace", "pillsbury",
               "progresso", "pure wesson", "pompeian", "san marzano", 
               "sargento", "soy vay", "taco bell", "yoplait", "spice islands", 
               "stonefire", "success", "swanson", "truvía", "uncle bens", "wish bone", 
               "zatarains", "morton", "jameson", "tapatio", "mountain high", 
               "philadelphia", "king arthur", "roma")

keywords = c("lowfat", "light", "shredded", "sliced", "all purpose", "allpurpose",
             "all natural", 
             "natural", "original", "gourmet", "traditional", "boneless", "skinless", 
             "fresh", "nonfat", "pitted", "quick cooking", "unbleached", "part skim", 
             "skim", "quickcooking", "oven ready", "homemade", "instant", "small",
             "extra large", "large", "chopped", "grated", "cooked", "stone ground", 
             "freshly ground", "ground", "pure", "peeled", "deveined", "organic", 
             "cracked", "granulated", "inch thick", "extra firm", "crushed", "flakes", 
             "self rising", "diced", "crumbles", "crumbled", "whole wheat", "whole grain", 
             "baby", "medium", "plain", "of", "thick cut", "cubed", "coarse", "free range",
             "seasoned", "canned", "multipurpose", "vegan", "thawed", "squeezed",
             "vegetarian", "fine", "zesty", "halves", "firmly packed", "drain", "drained",
             "washed")

measurements = c("in", "inch", "cm", "centimeter", "oz", "ounce", "l", "liter", "ml", 
                 "mililiter", "g", "gram", "cup", "gallon", "quart", "lb", "pound", "tbsp",
                 "tablespoon", "tsp", "teaspoon", "pint", "fl oz", "fluid ounce")

# Remove enlish stop words (ex. a, an, the) and the above words from the corpus of terms
corpus.cleaned <- tm_map(corpus.cleaned,removeWords, c(stopwords("english"),
                                                       brandnames, keywords, measurements))   
# corpus.cleaned has 49,718 elements, but its size is 209 MB

# Reduce the terms into root form
corpus.cleaned <- tm_map(corpus.cleaned, stemDocument) 
# corpus.cleaned has same number of elements, but the size is now 208.5 MB
corpus.cleaned[[1]]
# <<PlainTextDocument (metadata: 7)>>
#   romain lettuc
# black oliv
# grape tomato
# garlic
# pepper
# onion
# season
# garbanzo bean
# feta chees

# Remove white space from the corpus of terms
corpus.cleaned <- tm_map(corpus.cleaned,stripWhitespace)
# no change in elements or size, and the size is same 208.5 MB
corpus.cleaned[[1]]
# <<PlainTextDocument (metadata: 7)>>
#   romain lettuc
# black oliv
# grape tomato
# garlic
# pepper
# onion
# season
# garbanzo bean
# feta chees

# Create a Document Term Matrix from the corpus
dtm.ingredients <- DocumentTermMatrix(corpus.cleaned)  
dim(dtm.ingredients) # 49718 x 2667 terms

save(corpus.cleaned, file=paste0(saved.obj, "corpus-new-cleaned-R-Object"))
rm(corpus.cleaned)

# Retain terms that occur in at least 0.0001 documents or 0.01% of documents
# Amongst 49,718 documents, terms that are in at least 497 documents are 
# retained
dtm.ingredients.spr <- removeSparseTerms(dtm.ingredients, 0.9999) 
dim(dtm.ingredients.spr)  # 49718 x 1661 terms
# [1] 49718  1661

save(dtm.ingredients, file=paste0(saved.obj, "dtm-ingredients-new-R-Object"))
rm(dtm.ingredients)

# Inspect the Document Term Matrix
dtm.ingredients.spr
# <<DocumentTermMatrix (documents: 49718, terms: 1661)>>
#   Non-/sparse entries: 806576/81775022
# Sparsity           : 99%
# Maximal term length: 13
# Weighting          : term frequency (tf)

# Inspect the first element/row in the Document Term Matrix
dtm.ingredients.spr[[1]]
# [1]   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   2   2   2   2   2

# Inspect the column names of the Document Term Matrix (ingredient terms)
colnames(dtm.ingredients.spr)
# OUTPUT:
# > colnames(dtm.ingredients.spr)
# [1] "açai"          "acai"          "accent"        "achiot"        "acid"         
# [6] "acke"          "acorn"         "act"           "activ"         "adobo"        
# [11] "adzuki"        "agar"          "agav"          "age"           "ahi"          
# [16] "aioli"         "ajwain"        "aka"           "albacor"       "ale"          
# [21] "aleppo"        "alfalfa"       "alfredo"       "all"           "allspic"      
# [26] "almond"        "amaretti"      "amaretto"      "amchur"        "american"     
# [31] "amino"         "anaheim"       "ancho"         "anchovi"       "andouill"     
# [36] "anejo"         "angel"         "anglais"       "angostura"     "anis"         
# [41] "anjou"         "annatto"       "appl"          "apple"         "applesauc"    
# [46] "applewood"     "apricot"       "arbol"         "arborio"       "armagnac"     
# [51] "arrowroot"     "artichok"      "artisan"       "arugula"       "asada"        
# [56] "asadero"       "asafoetida"    "asiago"        "asian"         "asparagus"    
# [61] "assort"        "atta"          "avocado"       "back"          "bacon"        
# [66] "bag"           "baguett"       "bailey"        "bake"          "ball"         
# [71] "balm"          "balsam"        "balsamico"     "bamboo"        "banana"       
# [76] "banger"        "bar"           "barbecu"       "barilla"       "barley"       
# [81] "bartlett"      "base"          "basil"         "basmati"       "bass"         
# [86] "baton"         "batter"        "bawang"        "bay"           "bayleaf"      
# [91] "bbq"           "bean"          "beansprout"    "beaten"        "bechamel"     
# [96] "beef"          "beefsteak"     "beer"          "beet"          "belgian"      
# [101] "believ"        "bell"          "belli"         "ben"           "bengal"       
# [106] "bengali"       "beni"          "berri"         "bertolli"      "best"         
# [111] "bianco"        "bibb"          "bicarbon"      "bing"          "bird"         
# [116] "biryani"       "biscotti"      "biscuit"       "bisquick"      "bit"          
# [121] "bitter"        "bittersweet"   "black"         "blackberri"    "blacken"      
# [126] "blade"         "blanc"         "blanch"        "blanco"        "blend"        
# [131] "bliss"         "blood"         "bloodi"        "blossom"       "blue"         
# [136] "blueberri"     "boar"          "bocconcini"    "boil"          "bok"          
# [141] "bologna"       "bone"          "bonito"        "bonnet"        "borlotti"     
# [146] "bosc"          "boston"        "bottl"         "bottom"        "bought"       
# [151] "bouillon"      "bouquet"       "bourbon"       "bow"           "braeburn"     
# [156] "bragg"         "brais"         "bran"          "brand"         "brandi"       
# [161] "branzino"      "brat"          "bread"         "breadcrumb"    "breadstick"   
# [166] "breakfast"     "breast"        "brew"          "brie"          "brine"        
# [171] "brioch"        "brisket"       "broad"         "broccoli"      "broccolini"   
# [176] "broiler"       "broth"         "brown"         "browni"        "brussel"      
# [181] "bucatini"      "buckwheat"     "bud"           "buffalo"       "bulb"         
# [186] "bulgur"        "bulk"          "bun"           "burger"        "burgundi"     
# [191] "burrata"       "butt"          "butter"        "buttercream"   "butteri"      
# [196] "buttermilk"    "butternut"     "butterscotch"  "button"        "cabbag"       
# [201] "cabernet"      "cacao"         "cachaca"       "cactus"        "caesar"       
# [206] "cajun"         "cake"          "calamansi"     "calamari"      "calamata"     
# [211] "calf"          "california"    "calimyrna"     "callaloo"      "calori"       
# [216] "calvado"       "camembert"     "campanell"     "campari"       "campbel"      
# [221] "canadian"      "candi"         "cane"          "canela"        "cannellini"   
# [226] "cannelloni"    "cannoli"       "canola"        "cant"          "cantaloup"    
# [231] "canton"        "cap"           "capellini"     "caper"         "capicola"     
# [236] "capsicum"      "caramel"       "caraway"       "carbon"        "carcass"      
# [241] "cardamom"      "cardamon"      "caribbean"     "carn"          "carnaroli"    
# [246] "carnita"       "carrot"        "case"          "casera"        "cashew"       
# [251] "cassava"       "cassi"         "cassia"        "caster"        "catalina"     
# [256] "catfish"       "catsup"        "cauliflow"     "cavatappi"     "cavatelli"    
# [261] "caviar"        "cavolo"        "cayenn"        "celeri"        "celtic"       
# [266] "center"        "cereal"        "chaat"         "challa"        "cham"         
# [271] "chambord"      "champagn"      "chana"         "chanterell"    "chapati"      
# [276] "char"          "chard"         "chardonnay"    "chat"          "chayot"       
# [281] "cheddar"       "chee"          "chees"         "cheesi"        "chenpi"       
# [286] "cherri"        "cherryston"    "chervil"       "chestnut"      "chèvre"       
# [291] "chevr"         "chia"          "chianti"       "chicken"       "chickpea"     
# [296] "chiffonad"     "chihuahua"     "chile"         "chili"         "chiligarl"    
# [301] "chill"         "chilli"        "chines"        "chinkiang"     "chip"         
# [306] "chipotl"       "chive"         "chocol"        "choi"          "cholula"      
# [311] "chop"          "chorizo"       "chow"          "choy"          "chrysanthemum"
# [316] "chuck"         "chunk"         "chunki"        "chutney"       "ciabatta"     
# [321] "cider"         "cilantro"      "cinnamon"      "citric"        "citron"       
# [326] "citrus"        "clam"          "clarifi"       "classic"       "classico"     
# [331] "clear"         "clementin"     "clot"          "clove"         "club"         
# [336] "coar"          "cob"           "coca"          "cockl"         "cocktail"     
# [341] "coco"          "cocoa"         "coconut"       "cod"           "codfish"      
# [346] "coffe"         "cognac"        "cointreau"     "coke"          "cola"         
# [351] "colbi"         "cold"          "coleslaw"      "collar"        "collard"      
# [356] "collect"       "color"         "colour"        "comfort"       "compot"       
# [361] "concentr"      "condens"       "condiment"     "confit"        "consomm"      
# [366] "convert"       "cook"          "cooki"         "cool"          "coriand"      
# [371] "corn"          "cornbread"     "cornflak"      "cornflour"     "cornhusk"     
# [376] "cornichon"     "cornish"       "cornmeal"      "cotija"        "cottag"       
# [381] "countri"       "couscous"      "cover"         "crab"          "crabmeat"     
# [386] "cracker"       "cranberri"     "crawfish"      "cream"         "creami"       
# [391] "crema"         "crème"         "creme"         "cremini"       "creol"        
# [396] "crepe"         "crescent"      "cress"         "crimini"       "crisp"        
# [401] "crispi"        "crock"         "croissant"     "crookneck"     "crostini"     
# [406] "crouton"       "crumb"         "crush"         "crust"         "crusti"       
# [411] "crystal"       "cuban"         "cube"          "cucumb"        "cuisin"       
# [416] "cumin"         "cuminse"       "cur"           "curaçao"       "curd"         
# [421] "cure"          "curl"          "currant"       "curri"         "custard"      
# [426] "cut"           "cutlet"        "daikon"        "dairi"         "daisi"        
# [431] "dal"           "dandelion"     "daniel"        "dark"          "dash"         
# [436] "dashi"         "date"          "day"           "decor"         "deep"         
# [441] "deli"          "delici"        "demerara"      "demi"          "dend"         
# [446] "despelett"     "dessert"       "devein"        "devil"         "dhal"         
# [451] "dice"          "diet"          "digest"        "dijon"         "dill"         
# [456] "dillwe"        "dinner"        "dip"           "dish"          "distil"       
# [461] "ditalini"      "doenzang"      "dog"           "dolc"          "domino"       
# [466] "doubl"         "dough"         "dress"         "dri"           "drink"        
# [471] "drip"          "drummett"      "drumstick"     "duck"          "dulc"         
# [476] "dumpl"         "dung"          "dust"          "dutch"         "ear"          
# [481] "earl"          "eau"           "edam"          "edamam"        "edibl"        
# [486] "egg"           "eggplant"      "elbow"         "emeril"        "emmenthal"    
# [491] "empanada"      "enchilada"     "endiv"         "english"       "enokitak"     
# [496] "epazot"        "equal"         "escargot"      "escarol"       "espresso"     
# [501] "essenc"        "european"      "evapor"        "extra"         "extract"      
# [506] "eye"           "fajita"        "farina"        "farm"          "farmer"       
# [511] "farro"         "fashion"       "fat"           "fatback"       "fatfre"       
# [516] "fava"          "fed"           "feet"          "fennel"        "fenugreek"    
# [521] "ferment"       "feta"          "fettuccin"     "fettucin"      "fideo"        
# [526] "field"         "fiesta"        "fig"           "file"          "filet"        
# [531] "fill"          "fillet"        "filo"          "fine"          "finger"       
# [536] "fingerl"       "fire"          "firm"          "fish"          "fishcak"      
# [541] "five"          "flake"         "flank"         "flanken"       "flat"         
# [546] "flatbread"     "flavor"        "flax"          "flaxse"        "flesh"        
# [551] "fleur"         "flora"         "floret"        "flounder"      "flour"        
# [556] "flower"        "focaccia"      "foie"          "fontina"       "food"         
# [561] "forest"        "four"          "fraîch"        "frambois"      "frangelico"   
# [566] "frank"         "free"          "fregola"       "french"        "fresca"       
# [571] "fresco"        "fresh"         "fresno"        "fri"           "frise"        
# [576] "frito"         "fromag"        "frond"         "frost"         "frozen"       
# [581] "fruit"         "fryer"         "fudg"          "fuji"          "full"         
# [586] "fulli"         "fungus"        "furikak"       "fusilli"       "fuyu"         
# [591] "gaeta"         "gai"           "gala"          "galang"        "gallo"        
# [596] "game"          "garam"         "garbanzo"      "garden"        "gari"         
# [601] "garlic"        "garni"         "garnish"       "gelatin"       "gelato"       
# [606] "gemelli"       "genoa"         "germ"          "ghee"          "gherkin"      
# [611] "giardiniera"   "giblet"        "gin"           "ginger"        "gingerroot"   
# [616] "gingersnap"    "gizzard"       "glace"         "glass"         "glaze"        
# [621] "globe"         "gluten"        "glutin"        "gnocchi"       "goat"         
# [626] "gochugaru"     "gochujang"     "goji"          "gold"          "golden"       
# [631] "goos"          "gooseberri"    "goreng"        "gorgonzola"    "gouda"        
# [636] "gourd"         "gourmet"       "grade"         "graham"        "grain"        
# [641] "gran"          "grana"         "grand"         "granni"        "granola"      
# [646] "granul"        "grape"         "grapefruit"    "grapese"       "grappa"       
# [651] "gras"          "grass"         "grate"         "gravi"         "greas"        
# [656] "great"         "greater"       "greek"         "green"         "gremolata"    
# [661] "grenadin"      "grey"          "grill"         "grit"          "ground"       
# [666] "groundnut"     "grouper"       "gruyer"        "gruyèr"        "guacamol"     
# [671] "guajillo"      "guancial"      "guava"         "guin"          "gum"          
# [676] "gumbo"         "gyoza"         "haa"           "habanero"      "haddock"      
# [681] "hair"          "half"          "halibut"       "ham"           "hamburg"      
# [686] "hand"          "hanger"        "hanh"          "hanout"        "hard"         
# [691] "haricot"       "harina"        "harissa"       "hash"          "hass"         
# [696] "hatch"         "havarti"       "hazelnut"      "head"          "heart"        
# [701] "heavi"         "heinz"         "heirloom"      "hellmann"      "hemp"         
# [706] "hen"           "herb"          "herdez"        "herring"       "hibiscus"     
# [711] "hidden"        "high"          "himalayan"     "hoagi"         "hock"         
# [716] "hog"           "hoisin"        "holi"          "hollandais"    "home"         
# [721] "homestyl"      "homini"        "honey"         "honeydew"      "horseradish"  
# [726] "hot"           "hothous"       "hsing"         "hummus"        "hungarian"    
# [731] "husk"          "ice"           "iceberg"       "idaho"         "imit"         
# [736] "imo"           "indian"        "irish"         "iron"          "island"       
# [741] "italian"       "its"           "jack"          "jackfruit"     "jaggeri"      
# [746] "jalap"         "jalapeno"      "jam"           "jamaican"      "jambalaya"    
# [751] "japanes"       "jarlsberg"     "jasmin"        "jeera"         "jell"         
# [756] "jelli"         "jerk"          "jerki"         "jerusalem"     "jicama"       
# [761] "johnsonvill"   "juic"          "jumbo"         "junip"         "jus"          
# [766] "kabocha"       "kaffir"        "kahlua"        "kahlúa"        "kaiser"       
# [771] "kalamata"      "kale"          "kalonji"       "karo"          "kasha"        
# [776] "kasseri"       "kasuri"        "kecap"         "kefalotyri"    "kefir"        
# [781] "kelp"          "kernel"        "ketchup"       "kewpi"         "kewra"        
# [786] "key"           "khoa"          "kidnei"        "kidney"        "kielbasa"     
# [791] "kikkoman"      "kim"           "kimchi"        "king"          "kirbi"        
# [796] "kirsch"        "kitchen"       "kiwi"          "knockwurst"    "knorr"        
# [801] "kochujang"     "kombu"         "konbu"         "konnyaku"      "korean"       
# [806] "kosher"        "kraft"         "kumquat"       "lacinato"      "ladi"         
# [811] "ladyfing"      "lager"         "lamb"          "lan"           "lard"         
# [816] "lardon"        "lasagn"        "lasagna"       "laurel"        "lavend"       
# [821] "lea"           "leaf"          "leafi"         "lean"          "leav"         
# [826] "lech"          "leek"          "leg"           "lemon"         "lemonad"      
# [831] "lemongrass"    "lentil"        "less"          "lettuc"        "light"        
# [836] "lili"          "lima"          "lime"          "limead"        "limoncello"   
# [841] "linguin"       "linguini"      "link"          "lipton"        "liqueur"      
# [846] "liquid"        "liquor"        "lite"          "littleneck"    "liver"        
# [851] "loaf"          "loav"          "lobster"       "loin"          "long"         
# [856] "loos"          "lotus"         "louisiana"     "low"           "lower"        
# [861] "lowsodium"     "luke"          "lump"          "lumpia"        "lyche"        
# [866] "macadamia"     "macaroni"      "mace"          "machin"        "mackerel"     
# [871] "madeira"       "madra"         "maggi"         "mahi"          "mahimahi"     
# [876] "maida"         "maldon"        "malt"          "maltos"        "mam"          
# [881] "manchego"      "mandarin"      "mango"         "mani"          "manicotti"    
# [886] "manioc"        "manzanilla"    "mapl"          "margarin"      "margarita"    
# [891] "mari"          "marin"         "marinad"       "marinara"      "marjoram"     
# [896] "marmalad"      "marmit"        "marnier"       "marrow"        "marsala"      
# [901] "marshmallow"   "marzano"       "marzipan"      "masa"          "masala"       
# [906] "mascarpon"     "mash"          "masoor"        "massaman"      "matcha"       
# [911] "matur"         "matzo"         "mayer"         "mayonais"      "mayonnai"     
# [916] "mayonnais"     "mazola"        "mccormick"     "meal"          "meat"         
# [921] "meatbal"       "meatloaf"      "medal"         "medjool"       "mein"         
# [926] "melon"         "melt"          "merguez"       "meringu"       "merlot"       
# [931] "merluza"       "mesclun"       "methi"         "mex"           "mexican"      
# [936] "mexicana"      "mexicorn"      "meyer"         "mezcal"        "mignon"       
# [941] "mild"          "milk"          "millet"        "min"           "minc"         
# [946] "mincemeat"     "mini"          "miniatur"      "minicub"       "mint"         
# [951] "minut"         "miracl"        "mirin"         "mirliton"      "miso"         
# [956] "mitsuba"       "mix"           "mms"           "mochiko"       "moistur"      
# [961] "molass"        "mole"          "monkfish"      "monterey"      "moong"        
# [966] "morel"         "morsel"        "mortadella"    "mountain"      "mozzarella"   
# [971] "mrs"           "msg"           "muenster"      "muffin"        "mulato"       
# [976] "mullet"        "mung"          "muscovado"     "mushroom"      "mussel"       
# [981] "mustard"       "mutton"        "naan"          "nacho"         "nam"          
# [986] "napa"          "natur"         "navel"         "navi"          "neck"         
# [991] "nectar"        "nectarin"      "negro"         "nero"          "neufchâtel"   
# [996] "neutral"       "new"           "niçois"        "nigella"       "nilla"        
# [1001] "noir"          "non"           "nonstick"      "noodl"         "nopal"        
# [1006] "nopalito"      "nori"          "northern"      "not"           "nuoc"         
# [1011] "nut"           "nutella"       "nutmeg"        "nutrit"        "oat"          
# [1016] "oatmeal"       "octopus"       "oil"           "okra"          "old"          
# [1021] "olek"          "oliv"          "olive"         "oloroso"       "onion"        
# [1026] "orang"         "orecchiett"    "oregano"       "oreo"          "origin"       
# [1031] "orzo"          "oscar"         "ouzo"          "oxtail"        "oyster"       
# [1036] "pace"          "pack"          "padano"        "pak"           "palm"         
# [1041] "pan"           "pancak"        "pancetta"      "pancit"        "pandan"       
# [1046] "pandanus"      "paneer"        "panela"        "panetton"      "panko"        
# [1051] "papaya"        "paper"         "pappadam"      "pappardell"    "paprika"      
# [1056] "parboil"       "parma"         "parmagiano"    "parmesan"      "parmigiana"   
# [1061] "parmigiano"    "parsley"       "parsnip"       "part"          "pasilla"      
# [1066] "paso"          "pasoT"         "passata"       "passion"       "past"         
# [1071] "pasta"         "pastri"        "pate"          "pati"          "pattypan"     
# [1076] "pea"           "peach"         "peanut"        "pear"          "pearl"        
# [1081] "peasant"       "pecan"         "pecorino"      "pectin"        "peel"         
# [1086] "penn"          "peperoncini"   "peperoncino"   "pepita"        "pepper"       
# [1091] "peppercorn"    "peppermint"    "pepperoncini"  "pepperoni"     "perciatelli"  
# [1096] "perilla"       "pernod"        "perrin"        "persian"       "persimmon"    
# [1101] "pesto"         "petal"         "petit"         "philadelphia"  "phyllo"       
# [1106] "picant"        "picholin"      "pickl"         "pico"          "pie"          
# [1111] "piec"          "piecrust"      "pierogi"       "pig"           "pigeon"       
# [1116] "pilaf"         "pillsburyT"    "piloncillo"    "piment"        "pimento"      
# [1121] "pimenton"      "pine"          "pineappl"      "pinenut"       "pink"         
# [1126] "pinot"         "pinto"         "pistachio"     "pita"          "pizza"        
# [1131] "pla"           "plantain"      "plum"          "poblano"       "pocket"       
# [1136] "pod"           "poha"          "point"         "polenta"       "polish"       
# [1141] "pomegran"      "pomelo"        "pompeian"      "ponzu"         "popcorn"      
# [1146] "poppi"         "porcini"       "pork"          "porridg"       "port"         
# [1151] "portabello"    "porterhous"    "portobello"    "posol"         "pot"          
# [1156] "potato"        "poultri"       "powder"        "pralin"        "prawn"        
# [1161] "prebak"        "prego"         "premium"       "prepar"        "preserv"      
# [1166] "press"         "process"       "progresso"     "promis"        "prosciutto"   
# [1171] "prosecco"      "protein"       "provenc"       "provolon"      "prune"        
# [1176] "pud"           "puf"           "puff"          "pulp"          "pumpernickel" 
# [1181] "pumpkin"       "pumpkinse"     "puré"          "pure"          "purpo"        
# [1186] "purpos"        "puy"           "quail"         "quarter"       "queso"        
# [1191] "quick"         "quinc"         "quinoa"        "rabbit"        "rabe"         
# [1196] "rack"          "radicchio"     "radish"        "ragu"          "rainbow"      
# [1201] "rais"          "raisin"        "raita"         "rajma"         "ramen"        
# [1206] "ramp"          "ranch"         "rapese"        "rapid"         "ras"          
# [1211] "rasher"        "raspberri"     "ravioli"       "ravva"         "raw"          
# [1216] "readi"         "real"          "recip"         "red"           "redhot"       
# [1221] "reduc"         "reduct"        "refri"         "refriger"      "reggiano"     
# [1226] "regular"       "relish"        "remoulad"      "render"        "rhubarb"      
# [1231] "rib"           "rice"          "rich"          "ricotta"       "riesl"        
# [1236] "rigat"         "rigatoni"      "rin"           "rind"          "ring"         
# [1241] "rioja"         "ripe"          "ripen"         "rise"          "risotto"      
# [1246] "ritz"          "roast"         "rock"          "rocket"        "roe"          
# [1251] "roll"          "romain"        "romano"        "rome"          "root"         
# [1256] "roquefort"     "rosé"          "rose"          "rosemari"      "rosewat"      
# [1261] "rotel"         "rotell"        "rotini"        "rotisseri"     "rouill"       
# [1266] "round"         "roux"          "rub"           "rubi"          "rum"          
# [1271] "rump"          "runni"         "russet"        "russian"       "rutabaga"     
# [1276] "rye"           "safflow"       "saffron"       "sage"          "sago"         
# [1281] "saigon"        "sake"          "salad"         "salami"        "salata"       
# [1286] "salmon"        "salsa"         "salt"          "saltin"        "sambal"       
# [1291] "sambuca"       "san"           "sand"          "sandwich"      "sansho"       
# [1296] "santo"         "sardin"        "sargento"      "satsuma"       "sauc"         
# [1301] "sauerkraut"    "sausag"        "sautern"       "sauvignon"     "savori"       
# [1306] "savoy"         "sazon"         "scallion"      "scallop"       "schmaltz"     
# [1311] "schnapp"       "scotch"        "scrub"         "sea"           "seafood"      
# [1316] "season"        "seawe"         "sec"           "seca"          "seed"         
# [1321] "seedless"      "segment"       "seitan"        "sel"           "self"         
# [1326] "seltzer"       "semi"          "semisweet"     "semolina"      "serrano"      
# [1331] "sesam"         "shahi"         "shallot"       "shanghai"      "shank"        
# [1336] "shao"          "shaox"         "sharp"         "shave"         "sheep'"       
# [1341] "sheet"         "shell"         "shellfish"     "sherri"        "shichimi"     
# [1346] "shiitak"       "shimeji"       "shirataki"     "shiro"         "shiso"        
# [1351] "shoepeg"       "shoga"         "shoot"         "short"         "shortbread"   
# [1356] "shortcrust"    "shorten"       "shoulder"      "shoyu"         "shred"        
# [1361] "shrimp"        "shuck"         "sichuan"       "sicilian"      "sidesT"       
# [1366] "side"          "silken"        "silver"        "simpl"         "singl"        
# [1371] "sirloin"       "siu"           "size"          "skate"         "skim"         
# [1376] "skin"          "skirt"         "slab"          "slaw"          "slice"        
# [1381] "slider"        "sliver"        "smith"         "smoke"         "smooth"       
# [1386] "snail"         "snapper"       "snow"          "soak"          "soba"         
# [1391] "soda"          "sodium"        "sofrito"       "soft"          "soften"       
# [1396] "sole"          "solid"         "somen"         "sooji"         "soppressata"  
# [1401] "sorbet"        "sorghum"       "sorrel"        "soup"          "sour"         
# [1406] "sourdough"     "southern"      "soy"           "soya"          "soybean"      
# [1411] "soymilk"       "spaghetti"     "spaghettini"   "spam"          "spanish"      
# [1416] "spare"         "sparerib"      "sparkl"        "spear"         "spearmint"    
# [1421] "spelt"         "spice"         "spici"         "spike"         "spinach"      
# [1426] "spiral"        "splenda"       "split"         "spong"         "spray"        
# [1431] "spread"        "sprig"         "spring"        "sprinkl"       "sprite"       
# [1436] "sprout"        "squash"        "squid"         "squirt"        "sriracha"     
# [1441] "star"          "starch"        "starchi"       "steak"         "steam"        
# [1446] "steamer"       "steel"         "stem"          "stevia"        "stew"         
# [1451] "stick"         "sticki"        "stilton"       "stir"          "stock"        
# [1456] "stone"         "stonefir"      "store"         "stout"         "straw"        
# [1461] "strawberri"    "streaki"       "string"        "strip"         "stripe"       
# [1466] "strong"        "stuf"          "style"         "sub"           "substitut"    
# [1471] "suet"          "sugar"         "sugarcan"      "sultana"       "sum"          
# [1476] "sumac"         "summer"        "sunda"         "sundri"        "sunflow"      
# [1481] "superfin"      "sushi"         "swanson"       "sweet"         "sweeten"      
# [1486] "swiss"         "swordfish"     "syd"           "syrup"         "szechwan"     
# [1491] "tabasco"       "tabl"          "taco"          "tagliatell"    "tahini"       
# [1496] "tail"          "tamal"         "tamari"        "tamarind"      "tandoori"     
# [1501] "tangerin"      "tapenad"       "tapioca"       "taro"          "tarragon"     
# [1506] "tart"          "tartar"        "tasso"         "tater"         "tatsoi"       
# [1511] "tawni"         "tea"           "tempeh"        "tender"        "tenderloin"   
# [1516] "tentacl"       "tequila"       "teriyaki"      "tex"           "texa"         
# [1521] "thai"          "thaw"          "thick"         "thigh"         "thin"         
# [1526] "thousand"      "thread"        "thyme"         "tie"           "tiger"        
# [1531] "tilapia"       "tip"           "toast"         "toffe"         "tofu"         
# [1536] "togarashi"     "tom"           "tomatillo"     "tomato"        "ton"          
# [1541] "tongu"         "tonkatsu"      "toor"          "top"           "tortellini"   
# [1546] "tortilla"      "tostada"       "tot"           "tradit"        "treacl"       
# [1551] "tri"           "tripe"         "tripl"         "trout"         "truffl"       
# [1556] "tubetti"       "tumer"         "tuna"          "turbinado"     "turkei"       
# [1561] "turkey"        "turkish"       "turmer"        "turnip"        "turtl"        
# [1566] "twist"         "tzatziki"      "udon"          "ulek"          "ume"          
# [1571] "umeboshi"      "unbak"         "uncle"         "uncook"        "undrain"      
# [1576] "unflavor"      "unsalt"        "unseason"      "unsweeten"     "urad"         
# [1581] "valley"        "vanilla"       "varnish"       "vay"           "veal"         
# [1586] "veget"         "veggi"         "velveeta"      "venison"       "verbena"      
# [1591] "verd"          "vermicelli"    "vermouth"      "vert"          "vidalia"      
# [1596] "vie"           "vietnames"     "vin"           "vinaigrett"    "vine"         
# [1601] "vinegar"       "vineyard"      "virgin"        "vodka"         "wafer"        
# [1606] "waffl"         "wakam"         "walnut"        "warm"          "wasabi"       
# [1611] "water"         "watercress"    "watermelon"    "wax"           "waxi"         
# [1616] "wedg"          "weed"          "well"          "wheat"         "wheel"        
# [1621] "whip"          "whiskey"       "whiski"        "white"         "whitefish"    
# [1626] "whole"         "wholem"        "wide"          "wild"          "wine"         
# [1631] "wing"          "winter"        "wish"          "won"           "wondra"       
# [1636] "wonton"        "wood"          "worcestershir" "world"         "wrap"         
# [1641] "wrapper"       "xanthan"       "yakisoba"      "yam"           "yardlong"     
# [1646] "yeast"         "yellow"        "yellowfin"     "yogurt"        "yolk"         
# [1651] "york"          "young"         "yucca"         "yukon"         "yum"          
# [1656] "yuzu"          "zest"          "zesti"         "zinfandel"     "ziti"         
# [1661] "zucchini"     

# Find terms that are repeated at least 15000 times
findFreqTerms(dtm.ingredients.spr, 15000) # 8 terms
# OUTPUT:
# [1] "garlic" "oil"    "oliv"   "onion"  "pepper" "salt"   "sauc"   "sugar" 

# Find terms that are repeated at least 10000 times
findFreqTerms(dtm.ingredients.spr, 10000) # 19 terms
# OUTPUT:
# [1] "butter"  "chees"   "chicken" "chili"   "dri"     "egg"     "flour"   "garlic"  "oil"    
# [10] "oliv"    "onion"   "pepper"  "powder"  "red"     "salt"    "sauc"    "sugar"   "tomato" 
# [19] "water"  

# Create a Simple Matrix from the Sparse Document Term Matrix
dtm.mat <- as.matrix(dtm.ingredients.spr)

# get sum of rows for each term
dtm.mat.sum  <- colSums(dtm.mat)

# get the length of the columns of this matrix 
length(dtm.mat.sum)
# OUTPUT:
# [1] 1661

# Inspect the Simple Matrix
dtm.mat.sum
# OUTPUT:
# > dtm.mat.sum
# açai          acai        accent        achiot          acid          acke 
# 25             6             8            32             7            12 
# acorn           act         activ         adobo        adzuki          agar 
# 21            24           489           397             6            11 
# agav           age           ahi         aioli        ajwain           aka 
# 106            25            11            11            18             5 
# albacor           ale        aleppo       alfalfa       alfredo           all 
# 12            37            15             5            91            16 
# allspic        almond      amaretti      amaretto        amchur      american 
# 682          1452            21            43           108            56 
# amino       anaheim         ancho       anchovi      andouill         anejo 
# 43            66           308           413           358             7 
# angel       anglais     angostura          anis         anjou       annatto 
# 129             9            14           495            11            25 
# appl         apple     applesauc     applewood       apricot         arbol 
# 1040            12            32            20           308            60 
# arborio      armagnac     arrowroot      artichok       artisan       arugula 
# 363            19            43           364             5           283 
# asada       asadero    asafoetida        asiago         asian     asparagus 
# 5            31           200           104           316           419 
# assort          atta       avocado          back         bacon           bag 
# 7            10          1614            67          1532            92 
# baguett        bailey          bake          ball          balm        balsam 
# 420            12          3791            20             6           735 
# balsamico        bamboo        banana        banger           bar       barbecu 
# 7           174           392             7            33           176 
# barilla        barley      bartlett          base         basil       basmati 
# 10            70            23           263          3837           424 
# bass         baton        batter        bawang           bay       bayleaf 
# 92             9            21            30          1486          1075 
# bbq          bean    beansprout        beaten      bechamel          beef 
# 33          5020           585            70             8          3801 
# beefsteak          beer          beet       belgian        believ          bell 
# 29           380           170            49            26          5338 
# belli           ben        bengal       bengali          beni         berri 
# 159             8            18             8             6           163 
# bertolli          best        bianco          bibb      bicarbon          bing 
# 38            68             7            31            19             5 
# bird       biryani      biscotti       biscuit      bisquick           bit 
# 87            15             6           157            12            25 
# bitter   bittersweet         black    blackberri       blacken         blade 
# 45           190          2842           124             6             5 
# blanc        blanch        blanco         blend         bliss         blood 
# 18           131            18           313             6            26 
# bloodi       blossom          blue     blueberri          boar    bocconcini 
# 5            36           101           122             6            17 
# boil           bok       bologna          bone        bonito        bonnet 
# 783           252             5           297           116            54 
# borlotti          bosc        boston         bottl        bottom        bought 
# 7            27            75            77            16            64 
# bouillon       bouquet       bourbon           bow      braeburn         bragg 
# 308            30           265           140             6             7 
# brais          bran         brand        brandi      branzino          brat 
# 7            22             7           241             8             9 
# bread    breadcrumb    breadstick     breakfast        breast          brew 
# 2856           260            15            47          4159            92 
# brie         brine        brioch       brisket         broad      broccoli 
# 47            76            22            89            10           653 
# broccolini       broiler         broth         brown        browni       brussel 
# 27            20          5572          3775            10            47 
# bucatini     buckwheat           bud       buffalo          bulb        bulgur 
# 18            56            12            25           268            40 
# bulk           bun        burger      burgundi       burrata          butt 
# 28           156            17            35             9           161 
# butter   buttercream       butteri    buttermilk     butternut  butterscotch 
# 10842             7            10          1266           221             7 
# button        cabbag      cabernet         cacao       cachaca        cactus 
# 220          1549            12            12            95            10 
# caesar         cajun          cake     calamansi      calamari      calamata 
# 16           491           443            40            23             9 
# calf    california     calimyrna      callaloo        calori       calvado 
# 6            79            18            12            10            25 
# camembert     campanell       campari       campbel      canadian         candi 
# 5             7            12            27            27           159 
# cane        canela    cannellini    cannelloni       cannoli        canola 
# 79            22           256             5             5          1634 
# cant     cantaloup        canton           cap     capellini         caper 
# 26            30             7           134            14           682 
# capicola      capsicum       caramel       caraway        carbon       carcass 
# 7            85            54           111            23            12 
# cardamom      cardamon     caribbean          carn     carnaroli       carnita 
# 949            21             6            13            16            13 
# carrot          case        casera        cashew       cassava         cassi 
# 4061           150             5           448            13            11 
# cassia        caster      catalina       catfish        catsup     cauliflow 
# 15           200             8           155             9           394 
# cavatappi     cavatelli        caviar        cavolo        cayenn        celeri 
# 12            11            28             5          2678          2701 
# celtic        center        cereal         chaat        challa          cham 
# 9            72            37            50             7            12 
# chambord      champagn         chana    chanterell       chapati          char 
# 9            69            40            21             9            19 
# chard    chardonnay          chat        chayot       cheddar          chee 
# 158            13            21            41          2331           164 
# chees        cheesi        chenpi        cherri    cherryston       chervil 
# 14722             7            10           819             5            40 
# chestnut        chèvre         chevr          chia       chianti       chicken 
# 308            14            22            13            12         14451 
# chickpea     chiffonad     chihuahua         chile         chili     chiligarl 
# 588             9            14            12         12619            16 
# chill        chilli        chines     chinkiang          chip       chipotl 
# 5           182           901            27           709           720 
# chive        chocol          choi       cholula          chop       chorizo 
# 773          1088            17             5           445           322 
# chow          choy chrysanthemum         chuck         chunk        chunki 
# 37           271             7           303            68           184 
# chutney      ciabatta         cider      cilantro      cinnamon        citric 
# 141            59           833          7432          3263             6 
# citron        citrus          clam       clarifi       classic      classico 
# 16            14           424            69            10            23 
# clear     clementin          clot         clove          club          coar 
# 29             7             6          1349            58             6 
# cob          coca         cockl      cocktail          coco         cocoa 
# 15            14            10            68             9           479 
# coconut           cod       codfish         coffe        cognac     cointreau 
# 2767           139            29           305           117            35 
# coke          cola         colbi          cold      coleslaw        collar 
# 8            30            93           673            45            25 
# collard       collect         color        colour       comfort        compot 
# 197             6           143            44            14             6 
# concentr       condens     condiment        confit       consomm       convert 
# 153           678            14            21            19            24 
# cook         cooki          cool       coriand          corn     cornbread 
# 3078            95            15          2740          6738            69 
# cornflak     cornflour      cornhusk     cornichon       cornish      cornmeal 
# 32           129             8            40            34           630 
# cotija        cottag       countri      couscous         cover          crab 
# 171           225           207           265             6           331 
# crabmeat       cracker     cranberri      crawfish         cream        creami 
# 108           185           159           145          7708           151 
# crema         crème         creme       cremini         creol         crepe 
# 115           203            31           170           447            21 
# crescent         cress       crimini         crisp        crispi         crock 
# 57             5            82            13             9            24 
# croissant     crookneck      crostini       crouton         crumb         crush 
# 13            12            10            59          1242            27 
# crust        crusti       crystal         cuban          cube        cucumb 
# 323            62            62            15           316          1475 
# cuisin         cumin       cuminse           cur       curaçao          curd 
# 16          5813             5            30             5           152 
# cure          curl       currant         curri       custard           cut 
# 108            10           187          2046            20           129 
# cutlet        daikon         dairi         daisi           dal     dandelion 
# 131           192            10             9           230            12 
# daniel          dark          dash         dashi          date           day 
# 8          1343             6           197           171             5 
# decor          deep          deli        delici      demerara          demi 
# 13            10            46            50            11            29 
# dend     despelett       dessert        devein         devil          dhal 
# 8             9             8            24             5            11 
# dice          diet        digest         dijon          dill        dillwe 
# 9             5             5           711           559            43 
# dinner           dip          dish        distil      ditalini      doenzang 
# 34            99             7            24            33             7 
# dog          dolc        domino         doubl         dough         dress 
# 43             7             5           100           575           525 
# dri         drink          drip      drummett     drumstick          duck 
# 10258             9           125            11           159           178 
# dulc         dumpl          dung          dust         dutch           ear 
# 9            82            14            54            26           103 
# earl           eau          edam        edamam         edibl           egg 
# 6             7             7           104            12         11348 
# eggplant         elbow        emeril     emmenthal      empanada     enchilada 
# 757           116             8            11            12           516 
# endiv       english      enokitak        epazot         equal      escargot 
# 64           294            58            51            15             5 
# escarol      espresso        essenc      european        evapor         extra 
# 76           154            57             9           310           195 
# extract           eye        fajita        farina          farm        farmer 
# 2203           329            49             5            10            19 
# farro       fashion           fat       fatback        fatfre          fava 
# 27            26          3257            15            15            53 
# fed          feet        fennel     fenugreek       ferment          feta 
# 5            16           821           222            47           825 
# fettuccin      fettucin         fideo         field        fiesta           fig 
# 75           189            10            22            10           120 
# file         filet          fill        fillet          filo          fine 
# 81            66            46          1394             9           664 
# finger       fingerl          fire          firm          fish       fishcak 
# 63            28            21           301          2313            12 
# five         flake         flank       flanken          flat     flatbread 
# 358            81           321             9          1391            37 
# flavor          flax        flaxse         flesh         fleur         flora 
# 205            18            10            12            35            19 
# floret      flounder         flour        flower      focaccia          foie 
# 401            21         11107            90            15            16 
# fontina          food        forest          four        fraîch      frambois 
# 175           266            12            16           189             6 
# frangelico         frank          free       fregola        french        fresca 
# 15             8          1219             5           383            14 
# fresco         fresh        fresno           fri         frise         frito 
# 229          1321            18           142            22            12 
# fromag         frond         frost        frozen         fruit         fryer 
# 7            44            52          1991           168            42 
# fudg          fuji          full         fulli        fungus       furikak 
# 10            11            41            22             9            10 
# fusilli          fuyu         gaeta           gai          gala        galang 
# 65             6             7            48            14           107 
# gallo          game         garam      garbanzo        garden          gari 
# 107            15          1183           185            31            57 
# garlic         garni       garnish       gelatin        gelato       gemelli 
# 23624            23            11           181             5            18 
# genoa          germ          ghee       gherkin   giardiniera        giblet 
# 35            19           403            17            12            15 
# gin        ginger    gingerroot    gingersnap       gizzard         glace 
# 25          6735           143            16             8            39 
# glass         glaze         globe        gluten        glutin       gnocchi 
# 54            43             5           116            70            75 
# goat     gochugaru     gochujang          goji          gold        golden 
# 311            36           178            12           318           520 
# goos    gooseberri        goreng    gorgonzola         gouda         gourd 
# 9             6            30            84            41             8 
# gourmet         grade        graham         grain          gran         grana 
# 8             9            95           955            19            11 
# grand        granni       granola        granul         grape    grapefruit 
# 39           146            17           138           412            73 
# grapese        grappa          gras         grass         grate         gravi 
# 94            14            16           139           146            69 
# greas         great       greater         greek         green     gremolata 
# 32            61             6           629          6968             6 
# grenadin          grey         grill          grit        ground     groundnut 
# 18             7            20           386            10            12 
# grouper        gruyer        gruyèr      guacamol      guajillo      guancial 
# 20           135            77           242           119            27 
# guava          guin           gum         gumbo         gyoza           haa 
# 11            34            21            24            45            10 
# habanero       haddock          hair          half       halibut           ham 
# 85            34           119           909           127           848 
# hamburg          hand        hanger          hanh        hanout          hard 
# 94            18            10             7            53           168 
# haricot        harina       harissa          hash          hass         hatch 
# 60           122            82            28            54             9 
# havarti      hazelnut          head         heart         heavi         heinz 
# 11           189            23           317          1744             8 
# heirloom      hellmann          hemp           hen          herb        herdez 
# 42            71             8            46           345            14 
# herring      hibiscus        hidden          high     himalayan         hoagi 
# 5            10            15             5            11            26 
# hock           hog        hoisin          holi    hollandais          home 
# 137             9           519             7             5             6 
# homestyl        homini         honey      honeydew   horseradish           hot 
# 5           188          1757            22           164          2380 
# hothous         hsing        hummus     hungarian          husk           ice 
# 54             9            26            83           112           767 
# iceberg         idaho          imit           imo        indian         irish 
# 217            19            27             9             6            92 
# iron        island       italian           its          jack     jackfruit 
# 11            14          1712            24          1204            17 
# jaggeri         jalap      jalapeno           jam      jamaican     jambalaya 
# 49            18          2223           124            52            10 
# japanes     jarlsberg        jasmin         jeera          jell         jelli 
# 173             6           207            54            11            56 
# jerk         jerki     jerusalem        jicama   johnsonvill          juic 
# 71             5             7           110            28          8974 
# jumbo         junip           jus       kabocha        kaffir        kahlua 
# 138            30             7            11           175             6 
# kahlúa        kaiser      kalamata          kale       kalonji          karo 
# 23            13           514           247             6             5 
# kasha       kasseri        kasuri         kecap    kefalotyri         kefir 
# 5             5            67            23             9             5 
# kelp        kernel       ketchup         kewpi         kewra           key 
# 36           813           645             7            12            49 
# khoa        kidnei        kidney      kielbasa      kikkoman           kim 
# 9            26           373            63            12             5 
# kimchi          king         kirbi        kirsch       kitchen          kiwi 
# 154            25            31            22             5            43 
# knockwurst         knorr     kochujang         kombu         konbu      konnyaku 
# 5            27             8            19            88             5 
# korean        kosher         kraft       kumquat      lacinato          ladi 
# 48          4015            56            22            11             9 
# ladyfing         lager          lamb           lan          lard        lardon 
# 52            38           726            47           209             8 
# lasagn       lasagna        laurel        lavend           lea          leaf 
# 25           357             5            25             5          1526 
# leafi          lean          leav          lech          leek           leg 
# 9           699          6705             9           565           354 
# lemon       lemonad    lemongrass        lentil          less        lettuc 
# 7289            33           450           434           683          1525 
# light          lili          lima          lime        limead    limoncello 
# 17            18            88          6203            26             6 
# linguin      linguini          link        lipton       liqueur        liquid 
# 314            12           111             8           277           156 
# liquor          lite    littleneck         liver          loaf          loav 
# 9            25            76           148            42            19 
# lobster          loin          long          loos         lotus     louisiana 
# 87           342           875            40            18            27 
# low         lower     lowsodium          luke          lump        lumpia 
# 2836           174            15             9           141            32 
# lyche     macadamia      macaroni          mace        machin      mackerel 
# 8            15           155           106             6            14 
# madeira         madra         maggi          mahi      mahimahi         maida 
# 41            45            22            44             9            20 
# maldon          malt        maltos           mam      manchego      mandarin 
# 8            66             9             6            78            50 
# mango          mani     manicotti        manioc    manzanilla          mapl 
# 438            23            38            14             9           178 
# margarin     margarita          mari         marin       marinad      marinara 
# 331            10             5            66           114           290 
# marjoram      marmalad        marmit       marnier        marrow       marsala 
# 197            61            13            58            13           148 
# marshmallow       marzano      marzipan          masa        masala     mascarpon 
# 78             6            10           174          1358           180 
# mash        masoor      massaman        matcha         matur         matzo 
# 87            12            11            17             6            16 
# mayer      mayonais      mayonnai     mayonnais        mazola     mccormick 
# 10           983            59           219             9            20 
# meal          meat       meatbal      meatloaf         medal       medjool 
# 493           801            47            10             9            28 
# mein         melon          melt       merguez       meringu        merlot 
# 72            45           343            11            21             9 
# merluza       mesclun         methi           mex       mexican      mexicana 
# 5            13            95             5           650            25 
# mexicorn         meyer        mezcal        mignon          mild          milk 
# 28            13             5            19           156          7356 
# millet           min          minc     mincemeat          mini      miniatur 
# 13            16          2639            16            89            12 
# minicub          mint         minut        miracl         mirin      mirliton 
# 17          1701            14             7           626            13 
# miso       mitsuba           mix           mms       mochiko       moistur 
# 345             9          1107             5             9             7 
# molass          mole      monkfish      monterey         moong         morel 
# 178            23            13           732            38            16 
# morsel    mortadella      mountain    mozzarella           mrs           msg 
# 121            11             5          1765             6            33 
# muenster        muffin        mulato        mullet          mung     muscovado 
# 11            57            13             6           160            31 
# mushroom        mussel       mustard        mutton          naan         nacho 
# 2990           234          2253            29            50             8 
# nam          napa         natur         navel          navi          neck 
# 22           291             5            69            37            19 
# nectar      nectarin         negro          nero    neufchâtel       neutral 
# 141            29             5             5             7            47 
# new        niçois       nigella         nilla          noir           non 
# 106            30            30            12            11           192 
# nonstick         noodl         nopal      nopalito          nori      northern 
# 60          1761            28             5           182            61 
# not          nuoc           nut       nutella        nutmeg        nutrit 
# 24            25           475            12          1358            34 
# oat       oatmeal       octopus           oil          okra           old 
# 139            17            18         29102           415           132 
# olek          oliv         olive       oloroso         onion         orang 
# 6         15242            34             5         24065          2308 
# orecchiett       oregano          oreo        origin          orzo         oscar 
# 57          3439             9            41           152            10 
# ouzo        oxtail        oyster          pace          pack        padano 
# 12            43           727            16           115            10 
# pak          palm           pan        pancak      pancetta        pancit 
# 17           204            19            32           216             5 
# pandan      pandanus        paneer        panela      panetton         panko 
# 6            23           160             6             7           265 
# papaya         paper      pappadam    pappardell       paprika       parboil 
# 78            99             5            30          2411            14 
# parma    parmagiano      parmesan    parmigiana    parmigiano       parsley 
# 5            17          3659             5           516          5176 
# parsnip          part       pasilla          paso         pasoT       passata 
# 87            24            50            23            46            30 
# passion          past         pasta        pastri          pate          pati 
# 19          3702          1538           442            12            11 
# pattypan           pea         peach        peanut          pear         pearl 
# 5          1841           463          2188           269           168 
# peasant         pecan      pecorino        pectin          peel          penn 
# 12           755           275            11           579           397 
# peperoncini   peperoncino        pepita        pepper    peppercorn    peppermint 
# 15             6            32         34027          1038             8 
# pepperoncini     pepperoni   perciatelli       perilla        pernod        perrin 
# 27           125            10            19            28             5 
# persian     persimmon         pesto         petal         petit  philadelphia 
# 26            11           271             7            10            34 
# phyllo        picant      picholin         pickl          pico           pie 
# 159            82            20           369           107           307 
# piec      piecrust       pierogi           pig        pigeon         pilaf 
# 154           100             7            31            15             6 
# pillsburyT    piloncillo        piment       pimento      pimenton          pine 
# 17            24             9           254            11            53 
# pineappl       pinenut          pink         pinot         pinto     pistachio 
# 497           435            48            16           266           190 
# pita         pizza           pla      plantain          plum       poblano 
# 203           431            14            50          1215           394 
# pocket           pod          poha         point       polenta        polish 
# 6           353            15             7           194            10 
# pomegran        pomelo      pompeian         ponzu       popcorn         poppi 
# 73             5             7            29            14           100 
# porcini          pork       porridg          port    portabello    porterhous 
# 139          3077             5            81            86             7 
# portobello         posol           pot        potato       poultri        powder 
# 28             9            17          3672            78         11495 
# pralin         prawn        prebak         prego       premium        prepar 
# 6           173            18            10             6           254 
# preserv         press       process     progresso        promis    prosciutto 
# 194             9            82             8             6           353 
# prosecco       protein       provenc      provolon         prune           pud 
# 16             5            80           199           106            78 
# puf          puff          pulp  pumpernickel       pumpkin     pumpkinse 
# 5           216            25             9           246            13 
# puré          pure         purpo        purpos           puy         quail 
# 294            48             7          6059             9            27 
# quarter         queso         quick         quinc        quinoa        rabbit 
# 35           276            22            17           189            27 
# rabe          rack     radicchio        radish          ragu       rainbow 
# 66            21            82           403            48             5 
# rais        raisin         raita         rajma         ramen          ramp 
# 10           891            10             9            93            16 
# ranch        rapese         rapid           ras        rasher     raspberri 
# 137            18            20            53             8           296 
# ravioli         ravva           raw         readi          real         recip 
# 63            15           150             7            55            14 
# red        redhot         reduc        reduct         refri      refriger 
# 10514             6          1191             6           358           225 
# reggiano       regular        relish      remoulad        render       rhubarb 
# 535            56            71             5            14            31 
# rib          rice          rich       ricotta         riesl         rigat 
# 1211          7684             8           903            14            22 
# rigatoni           rin          rind          ring         rioja          ripe 
# 108            32           239            15             6           122 
# ripen          rise       risotto          ritz         roast          rock 
# 49            25            28             7          1659            72 
# rocket           roe          roll        romain        romano          rome 
# 58             8           504           429           365             6 
# root     roquefort          rosé          rose      rosemari       rosewat 
# 711            16            12            56          1034             5 
# rotel        rotell        rotini     rotisseri        rouill         round 
# 37            43            80            86             6           219 
# roux           rub          rubi           rum          rump         runni 
# 13           108            17           302            30            17 
# russet       russian      rutabaga           rye       safflow       saffron 
# 340             6            22            40            54           610 
# sage          sago        saigon          sake         salad        salami 
# 563             9             6           427           437           119 
# salata        salmon         salsa          salt        saltin        sambal 
# 46           388          1692         30836            55            79 
# sambuca           san          sand      sandwich        sansho         santo 
# 5             6            10           116             6            11 
# sardin      sargento       satsuma          sauc    sauerkraut        sausag 
# 26             8            16         16490            41          1998 
# sautern     sauvignon        savori         savoy         sazon      scallion 
# 6            19            28            69            14          2443 
# scallop      schmaltz       schnapp        scotch         scrub           sea 
# 256             8            14            70            14          1857 
# seafood        season         seawe           sec          seca          seed 
# 65          2889            73            48             5          5126 
# seedless       segment        seitan           sel          self       seltzer 
# 126            14             6            35            10            22 
# semi     semisweet      semolina       serrano         sesam         shahi 
# 101           225           153           687          4392            23 
# shallot      shanghai         shank          shao         shaox         sharp 
# 1866             8           140             9           330           595 
# shave        sheep'         sheet         shell     shellfish        sherri 
# 34             5           184           551             5           780 
# shichimi       shiitak       shimeji     shirataki         shiro         shiso 
# 22           741             7            15            15            33 
# shoepeg         shoga         shoot         short    shortbread    shortcrust 
# 8             7           196           238             7             7 
# shorten      shoulder         shoyu         shred        shrimp         shuck 
# 445           525            29            33          2935            46 
# sichuan      sicilian        sidesT          side        silken        silver 
# 20            11             8             8            72            25 
# simpl         singl       sirloin           siu          size         skate 
# 50            21           354            13            22             6 
# skim          skin         skirt          slab          slaw         slice 
# 97            87            88            14           102           745 
# slider        sliver         smith         smoke        smooth         snail 
# 5           234           146          1078             5            20 
# snapper          snow          soak          soba          soda        sodium 
# 106           223             5           135          1318          2800 
# sofrito          soft        soften          sole         solid         somen 
# 5           204           244            20            26             9 
# sooji   soppressata        sorbet       sorghum        sorrel          soup 
# 5            17            12            18            17           601 
# sour     sourdough      southern           soy          soya       soybean 
# 2486           105            14          6090            17            48 
# soymilk     spaghetti   spaghettini          spam       spanish         spare 
# 9           550            33             7           190            17 
# sparerib        sparkl         spear     spearmint         spelt         spice 
# 48            15            77            10            18           747 
# spici         spike       spinach        spiral       splenda         split 
# 60            14          1719             5            37            82 
# spong         spray        spread         sprig        spring       sprinkl 
# 19          2513            83           988           613            75 
# sprite        sprout        squash         squid        squirt      sriracha 
# 14           214           562           115             8           380 
# star        starch       starchi         steak         steam       steamer 
# 326          2414             6          1157           123            10 
# steel          stem        stevia          stew         stick        sticki 
# 10           108            26           420           959            39 
# stilton          stir         stock         stone      stonefir         store 
# 34            56          2406             6             5            64 
# stout         straw    strawberri       streaki        string         strip 
# 34            39           311            12            27            47 
# stripe        strong          stuf         style           sub     substitut 
# 9            18           118           447            11           102 
# suet         sugar      sugarcan       sultana           sum         sumac 
# 27         15650             6            25            14             8 
# summer         sunda        sundri       sunflow      superfin         sushi 
# 42             5           280           143            93           111 
# swanson         sweet       sweeten         swiss     swordfish           syd 
# 13          2073           542           281            33             5 
# syrup      szechwan       tabasco          tabl          taco    tagliatell 
# 762           178           259            11           807            26 
# tahini          tail         tamal        tamari      tamarind      tandoori 
# 81            22             8           140           260            43 
# tangerin       tapenad       tapioca          taro      tarragon          tart 
# 41            20           136            22           309            68 
# tartar         tasso         tater        tatsoi         tawni           tea 
# 228            33            11             5            23           286 
# tempeh        tender    tenderloin       tentacl       tequila      teriyaki 
# 18            50           467            15           200            84 
# tex          texa          thai          thaw         thick         thigh 
# 5             6           906            12            16          1047 
# thin      thousand        thread         thyme           tie         tiger 
# 49             5           399          3082           133            35 
# tilapia           tip         toast         toffe          tofu     togarashi 
# 120            59          1117            18           794            37 
# tom     tomatillo        tomato           ton         tongu      tonkatsu 
# 6           416         13949            45            18            15 
# toor           top    tortellini      tortilla       tostada           tot 
# 17           281            95          3327            51            11 
# tradit        treacl           tri         tripe         tripl         trout 
# 10            12            14             9            48            46 
# truffl       tubetti         tumer          tuna     turbinado        turkei 
# 74             9           963           251            64            12 
# turkey       turkish        turmer        turnip         turtl         twist 
# 778            22          1218           197             6            10 
# tzatziki          udon          ulek           ume      umeboshi         unbak 
# 20            89            63             5             8            28 
# uncle        uncook       undrain      unflavor        unsalt      unseason 
# 8            61            14           132          3673             5 
# unsweeten          urad        valley       vanilla       varnish           vay 
# 683            84            15          2782            15             5 
# veal         veget         veggi      velveeta       venison       verbena 
# 239          7440            38            38            11             6 
# verd    vermicelli      vermouth          vert       vidalia           vie 
# 176           217            87            59           140             7 
# vietnames           vin    vinaigrett          vine       vinegar      vineyard 
# 85            11           103            67          6190             6 
# virgin         vodka         wafer         waffl         wakam        walnut 
# 20           101            87             7            21           420 
# warm        wasabi         water    watercress    watermelon           wax 
# 752           112         12226           140            58             7 
# waxi          wedg          weed          well         wheat         wheel 
# 39           900            25            14           178            13 
# whip       whiskey        whiski         white     whitefish         whole 
# 1360           357             6          9821            68          1876 
# wholem          wide          wild          wine          wing        winter 
# 6            38            96          5336           178            15 
# wish           won        wondra        wonton          wood worcestershir 
# 16            45             7           206            44           879 
# world          wrap       wrapper       xanthan      yakisoba           yam 
# 27            14           463            21            12            75 
# yardlong         yeast        yellow     yellowfin        yogurt          yolk 
# 28           961          1350             5          1827          1423 
# york         young         yucca         yukon           yum          yuzu 
# 24             6             5           273             6            11 
# zest         zesti     zinfandel          ziti      zucchini 
# 1129             7            13            51          1152 

dtm.mat[35067,colnames(dtm.mat) %in% c("black","pepper","chicken","onion","red")]

save(dtm.mat, file=paste0(saved.obj, "dtm-mat-new-R-Object"))
rm(dtm.mat)

# For every term (column), get the number of rows/recipes, the term is part of
mat.ord <- order(dtm.mat.sum)

# Inspect the column totals
mat.ord

#check least and most frequent terms
dtm.mat.sum[head(mat.ord)]  # this gives the least used terms
# aka alfalfa artisan   asada    bing   blade 
# 5       5       5       5       5       5 

dtm.mat.sum[tail(mat.ord)]  # this gives the most used terms
# sauc  garlic  onion    oil   salt pepper 
# 16490  23624  24065  29102  30836  34027 

#load(file=paste0(saved.obj, "dtm-mat-R-Object"))

# Create sets of popular and least terms for plotting
# Top 25 terms
popular.ingredients = data.frame(ingredients = names(dtm.mat.sum[tail(mat.ord, 25)]), 
                                 num_recipes=dtm.mat.sum[tail(mat.ord, 25)])
popular.ingredients$ingredients = reorder(popular.ingredients$ingredients, 
                                          popular.ingredients$num_recipes)

dev.off()
ggplot(popular.ingredients[1:25,], aes(x=ingredients, y=num_recipes)) + 
  geom_point(size=5, colour="red") + coord_flip() +
  ggtitle("Popularity of Top 25 Terms") + 
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), 
        axis.text.y=element_text(size=13,colour="black", face="bold"), 
        axis.title.x=element_text(size=14, face="bold"), 
        axis.title.y=element_text(size=14,face="bold"),
        plot.title=element_text(size=24,face="bold"))

# Least 25 terms
popular.ingredients = data.frame(ingredients = names(dtm.mat.sum[head(mat.ord, 25)]), 
                                 num_recipes=dtm.mat.sum[head(mat.ord, 25)])
popular.ingredients$ingredients = reorder(popular.ingredients$ingredients, 
                                          popular.ingredients$num_recipes)

dev.off()
ggplot(popular.ingredients[1:25,], aes(x=ingredients, y=num_recipes)) + 
  geom_point(size=5, colour="red") + coord_flip() +
  ggtitle("Popularity of Least 25 Terms") + 
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), 
        axis.text.y=element_text(size=13,colour="black", face="bold"), 
        axis.title.x=element_text(size=14, face="bold"), 
        axis.title.y=element_text(size=14,face="bold"),
        plot.title=element_text(size=24,face="bold"))

dev.off()

#check our table of 15 frequencies
#load(file=paste0(saved.obj, "mat-ord-R-Object"))
head(table(mat.ord),15)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 
# 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 

tail(table(mat.ord),15)
# 1647 1648 1649 1650 1651 1652 1653 1654 1655 1656 1657 1658 1659 1660 1661 
# 1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 

#find associated terms
findAssocs(dtm.ingredients.spr, c('salt','oil'), corlimit=0.30)
# OUTPUT:
# $salt
# numeric(0)
#
# $oil
# oliv  sesam  veget garlic 
# 0.47   0.35   0.34   0.31 

save(dtm.ingredients.spr, file=paste0(saved.obj,"dtm-ingredients-spr-new-R-Object"))
rm(dtm.ingredients.spr)

# Jumbled Chart
dev.off()
load(file=paste0(saved.obj, "dtm-ingredients-spr-R-Object"))
plot(dtm.ingredients.spr, 
     terms=findFreqTerms(dtm.ingredients.spr,lowfreq=10000)[1:15],
     corThreshold=0.15)

## Colored works better than the previous plot
# First create a Term Document Matrix, which is a transpose of the Document
# Term Matrix
tdm <- t(dtm.ingredients.spr)

list(
  color=c(futures="blue", demand="red"),
  shape=c(crude="ellipse", budget="circle")
)

# find 20 terms that are found at least 10,000 times within the recipes
freqTerms <- findFreqTerms(dtm.ingredients.spr, lowfreq=10000)[1:20]

x <- as.matrix(t(tdm[freqTerms,]))
y <- cor(x)
vtxcnt <- rowSums(y>=.15)-1

mycols <- c("#f7fcf0", "#deebf7", "#e0f3db", "#e0f3db", "#a8ddb5", 
            "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081")

vc <- mycols[vtxcnt+1]
names(vc) <- names(vtxcnt)

head(vc)

pp <- plot(tdm, 
           terms = freqTerms, 
           corThreshold = 0.15,
           nodeAttrs=list(fillcolor=vc))
rm(wf)
rm(x)
rm(y)
rm(freqTerms)
rm(vc)
rm(vtxcnt)
rm(pp)

#####################################################################
# TOPIC MODELLING
#####################################################################
# Use of 'stm' package for use functionality of Structure Topic Model
# from STM vignette
#library(stm)
#library(topicmodels)

rowTotals <- apply(t(tdm), 1, sum)
rowsum.dtm.ingredients.spr <- apply(dtm.ingredients.spr, 1, sum)
#dtm.def.clean = dtm.def.spr[rowsum.dtm.def.spr>0,]

# Use Latent Dirichlet Allocation from topicmodels package
lda <- LDA(dtm.ingredients.spr[rowsum.dtm.ingredients.spr>0,], 
           k=5, # at least find 5 topics
           method="Gibbs",
           control=list(nstart=5, seed=list(101,102,103,104,105),
                        best=TRUE, burnin=4000, iter=2000, thin=500))  
save(lda, file=paste0(saved.obj, "lda-R-Object"))
term <- terms(lda, 5)      # get first 5 terms of every topic
term
#      Topic 1   Topic 2  Topic 3    Topic 4 Topic 5 
# [1,] "pepper"  "sugar"  "ground"   "fresh" "oil"   
# [2,] "chicken" "salt"   "onion"    "oliv"  "sauc"  
# [3,] "onion"   "egg"    "chili"    "chees" "garlic"
# [4,] "red"     "flour"  "salt"     "oil"   "rice"  
# [5,] "black"   "butter" "cilantro" "dri"   "veget" 

#term <- apply(term, MARGIN=2, paste, collapse=", ")
#term
# Topic 1                              Topic 2 
# "oil, sauc, garlic, chicken, onion" "pepper, fresh, oliv, chees, tomato" 
# Topic 3 
# "salt, ground, sugar, egg, flour" 

topic <- topics(lda, 1) # Get 1 topic for each recipe
topicProbabilities <- as.data.frame(lda@gamma)
term.freq <- rowSums(as.matrix(dtm.ingredients.spr[rowsum.dtm.ingredients.spr>0,]))
topics <- data.frame(term.freq, topic)

dev.off()
qplot(term.freq, ..count.., data=topics, geom="histogram", color=term[topic], 
      binwidth=0.01,
      xlab="Total Terms in Recipes", 
      ylab="Documents Covered by Topics")

# Another plot to display total recipes per topic
dev.off()
barplot(table(topics$topic), 
        names.arg=c("Topic-1","Topic-2","Topic-3","Topic-4","Topic-5"), 
        col=rainbow(5), main="Total Recipes Per Topic", ylab="Recipes")

# For analysis of probabilities
df.topics.prob = as.data.frame(topicProbabilities)
df.topics.prob = cbind(recipes=as.character(rownames(df.topics.prob)), 
                       topic=df.topics.prob, stringsAsFactors=F)

sample(which(df.topics.prob$topic.V1 > .3), 10)
# [1]  8118 16270 45431 31782 43012 20011  8101  9939  8553 46977

#create a data frame for visualization for frequency of ingredients
wf <- data.frame(word = names(dtm.mat.sum), freq = dtm.mat.sum)
head(wf)
# word freq
# açai     açai   25
# acai     acai    6
# accent accent    8
# achiot achiot   32
# acid     acid    7
# acke     acke   12

#plot terms which appear atleast 15,000 times
#library(ggplot2)
#library(plyr)
#library(data.table)
dev.off()
## set the levels in order we want
wf.sub <- data.table(subset(wf, freq >15000))
wf.sub
# word  freq
# 1: garlic 23624
# 2:    oil 29102
# 3:   oliv 15242
# 4:  onion 24065
# 5: pepper 34027
# 6:   salt 30836
# 7:   sauc 16490
# 8:  sugar 15650

#wf.sub <- arrange(wf.sub, desc(freq))

chart <- ggplot(wf.sub, aes(x = reorder(word, -freq), y = freq))
chart <- chart + geom_bar(stat = 'identity', color = 'black', fill = 'white')
chart <- chart + theme(axis.text.x=element_text(angle=45, hjust=1))
chart

dev.off()
rm(wf.sub)
rm(chart)

#create wordcloud
#library(wordcloud)
dev.off()
set.seed(142)

#plot word cloud of terms having a minumum frequency of 50
wordcloud(names(dtm.mat.sum), dtm.mat.sum, min.freq = 100, scale = c(2, .1), 
          colors = brewer.pal(4, "BuPu"))

#plot 300 most used words
dev.off()
set.seed(143)
wordcloud(names(dtm.mat.sum), dtm.mat.sum, max.words = 300, scale = c(3, .3), 
          colors = brewer.pal(6, 'Dark2'))

dev.off()

save(dtm.mat.sum, file=paste0(saved.obj, "dtm-mat-sum-new-R-Object"))
save(mat.ord, file=paste0(saved.obj, "mat-ord-new-R-Object"))
save(lda, file=paste0(saved.obj, "lda-new-R-Object"))
save(df.topics.prob, file=paste0(saved.obj, "dt-topics-probabilities-new-R-Object"))
save(popular.ingredients, file=paste0(saved.obj, "popular-ingredients-new-R-Object"))
save(term, file=paste0(saved.obj, "term-new-R-Object"))
save(term.freq, file=paste0(saved.obj, "term-freq-new-R-Object"))
save(topicProbabilities, file=paste0(saved.obj, "topicProbabilities-new-R-Object"))
save(topics, file=paste0(saved.obj, "topics-new-R-Object"))
save(topic, file=paste0(saved.obj, "topic-new-R-Object"))
save(wf, file=paste0(saved.obj, "wf-new-R-Object"))
save(tdm, file=paste0(saved.obj, "tdm-new-R-Object"))

rm(dtm.mat.sum)
rm(mat.ord)
rm(lda)
rm(df.topics.prob)
rm(popular.ingredients)
rm(term)
rm(term.freq)
rm(topicProbabilities)
rm(topics)
rm(topic)
rm(wf)
rm(tdm)

rm(brandnames)
rm(keywords)
rm(measurements)
rm(phr)
rm(replacement.phrase)
rm(replacement.word)
rm(vec)

######################### DATA PREPARATION / EXPLORATION - #######################   


######################### FORM TRAINING/TEST DATA SETS - BEGIN #######################   

#library(tm)
df.cuisines <- as.data.frame(as.matrix(dtm.ingredients.spr))  
save(dtm.ingredients.spr, file=paste0(saved.obj, "dtm-ingredients-spr-new-R-Object"))
rm(dtm.ingredients.spr)

#df.cuisines <- sign(df.cuisines) 
traincleaned <- df.cuisines[1:39774,]    
testcleaned <- df.cuisines[39775:49718,]  
save(df.cuisines, file=paste0(saved.obj, "df-cuisines-new-R-Object"))
rm(df.cuisines)

traincleaned$cuisine <- train.dat$cuisine    
traincleaned$cuisine <- as.factor(traincleaned$cuisine) 
rm(train.dat)

save(traincleaned, file=paste0(saved.obj, "traincleaned-new-R-Object"))
save(testcleaned, file=paste0(saved.obj, "testcleaned-new-R-Object"))

table(traincleaned$cuisine)
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 467          804         1546         2673          755         2646         1175 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 3003          667         7838          526         1423          830         6438 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 821          489         4320          989         1539          825 

######################### FORM TRAINING/TEST DATA SETS - EN#######################   


######################### VALIDATION DATA SET - BEGIN ############################   

#library(caret)

# considering response variable as strata
data_part <- createDataPartition(y = traincleaned$cuisine, 
                                 p = 0.7, list = F)
train.samp.1 <- traincleaned[data_part,] # 70% here
train.samp.2 <- traincleaned[-data_part,] # 30% data goes here

train.data <- train.samp.1[,1:1661]
train.label <- train.samp.1[,1662]
valid.data <- train.samp.2[,1:1661]
valid.label <- train.samp.2[,1662]

nrow(train.samp.1)
# [1] 27852

nrow(train.samp.2)
# [1] 11922

table(train.samp.1$cuisine) # both training samples have all 20 cuisines
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 327          563         1083         1872          529         1853          823 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 2103          467         5487          369          997          581         4507 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 575          343         3024          693         1078          578 

table(train.samp.2$cuisine) # both training samples have all 20 cuisines
# > table(train.samp.2$cuisine) # both training samples have all 20 cuisines
# 
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 140          241          463          801          226          793          352 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 900          200         2351          157          426          249         1931 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 246          146         1296          296          461          247 
# > 

# number of rows of data that have missing values
sum(!complete.cases(train.samp.1))
#   > sum(!complete.cases(train.samp.1))
# [1] 0

sum(!complete.cases(train.samp.2))
# > sum(!complete.cases(train.samp.2))
# [1] 0

######################### VALIDATION DATA SET - END ##############################   


################# MEMORY SETTING - BEGIN #########################################

memory.size() # shows the current size of memory
memory.limit() # shows the current limit of memory
memory.limit(8192)  # set the current limit to 8 GB if Windows 64-bit

################# MEMORY SETTING - END ###########################################


################# SVM MODEL - BEGIN ##############################################

# svm is used to train a support vector machine. It can be used to carry out general
# regression and classification (of nu and epsilon-type), as well as 
# density-estimation. A formula interface is provided.

#library(e1071)

svm.pred <-svm(cuisine ~ ., data = train.samp.1)
# Warning message:
#   In svm.default(x, y, scale = scale, ..., na.action = na.action) :
#   Variable(s) 'cuminse' and 'gourmet' and 'texa' constant. Cannot scale data.

summary(svm.pred)
# Call:
#   svm(formula = cuisine ~ ., data = train.samp.1)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  0.000602047 
# 
# Number of Support Vectors:  23079
# 
# ( 327 563 1037 1424 529 1853 823 1625 467 3691 369 996 581 2890 575 343 2762 693 953 578 )
# 
# 
# Number of Classes:  20 
# 
# Levels: 
#   brazilian british cajun_creole chinese filipino french greek indian irish italian jamaican japanese korean mexican moroccan russian southern_us spanish thai vietnamese
# 

# #Predict Output 
predict.svm <- predict(svm.pred, valid.data)

table(valid.label)
# valid.label
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 140          241          463          801          226          793          352 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 900          200         2351          157          426          249         1931 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 246          146         1296          296          461          247 

table(predict.svm)
# predict.svm
# brazilian   british cajun_creole    chinese     filipino       french        greek       indian 
# 0            0          232         1191            0           35           12          827 
# irish   italian     jamaican     japanese       korean      mexican     moroccan      russian 
# 0         4199            0          162            1         2023           45            0 
# southern_us   spanish     thai   vietnamese 
# 2790            0          405            0 

confusionMatrix(train.samp.2$cuisine, predict.svm)
# Overall Statistics
# 
# Accuracy : 0.574          
# 95% CI : (0.565, 0.5829)
# No Information Rate : 0.3522         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.5062         
# Mcnemar's Test P-Value : NA             

# Use full training data
svm.pred <-svm(cuisine ~ ., data = traincleaned)
# Took more than 24 hours

summary(svm.pred)
# Call:
#   svm(formula = cuisine ~ ., data = traincleaned)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  0.000602047 
# 
# Number of Support Vectors:  26854
# 
# ( 874 3137 705 1675 419 928 4391 3012 1714 787 1090 765 1127 421 2397 1043 623 652 632 462 )
# 
# 
# Number of Classes:  20 
# 
# Levels: 
#   brazilian british cajun_creole chinese filipino french greek indian irish italian jamaican japanese korean mexican moroccan russian southern_us spanish thai vietnamese

# #Predict Output for the given test data file
predict.svm <- predict(svm.pred, testcleaned)
# predict.svm
# brazilian      british cajun_creole      chinese     filipino       french        greek       indian 
# 73           80          307          707           99          675          238          767 
# irish      italian     jamaican     japanese       korean      mexican     moroccan      russian 
# 77         2561           81          244          142         1641          149           58 
# southern_us      spanish         thai   vietnamese 
# 1409          118          383          135 

submission_svm <- data.frame(id=test.dat$id, cuisine=predict.svm)    
write.csv(submission_svm,file=paste0(file.path, "submission_svm_new.csv"),row.names=F)

# Kaggle Score/Ranking
# Score: 0.72848
# Rank: 1,023 / 1,388 (Bottom 26% or 26th percentile)

save(svm.pred, file=paste0(saved.obj, "svm-pred-new-R-Object"))
rm(svm.pred)
save(predict.svm, file=paste0(saved.obj, "predict-svm-new-R-Object"))
rm(predict.svm)
save(submission_svm, file=paste0(saved.obj, "submission-svm-new-R-Object"))
rm(submission_svm)

################# SVM MODEL - END ################################################

dtrain <- xgb.DMatrix(Matrix(
  data.matrix(traincleaned[,!colnames(traincleaned) %in% c('cuisine')])), 
  label = as.numeric(traincleaned$cuisine)-1)

#advanced data set preparation
dtest <- xgb.DMatrix(Matrix(
  data.matrix(testcleaned[,!colnames(testcleaned) %in% c('cuisine')]))) 
watchlist <- list(train = dtrain, test = dtest)

parm <- list("objective"="multi:softmax", "eval_metric"="merror",
             max.depth = 20, 
             eta = 0.1, 
             subsample = 1, 
             colsample_bytree = 1,
             num_class = 20, 
             verbose = 1,
             watchlist = watchlist)

set.seed(292)
xgbmodel <- xgb.cv(params=parm, data=dtrain, nrounds=750, prediction=TRUE, nfold=5)

tail(xgbmodel$dt)
# train.merror.mean train.merror.std test.merror.mean test.merror.std
# 1:          0.001163         0.000092         0.199050        0.004400
# 2:          0.001169         0.000103         0.198849        0.004330
# 3:          0.001150         0.000082         0.199000        0.004362
# 4:          0.001144         0.000093         0.198874        0.004419
# 5:          0.001137         0.000105         0.198774        0.004277
# 6:          0.001125         0.000098         0.198874        0.004429

min.error.idx <- which.min(xgbmodel$dt[, test.merror.mean])
min.error.idx
# [1] 499

xgbmodel$dt[min.error.idx,]
# train.merror.mean train.merror.std test.merror.mean test.merror.std
# 1:     0.001137         0.000105         0.198774        0.004277

confusionMatrix(factor(as.numeric(traincleaned$cuisine)-1), factor(xgbmodel$pred))
# Overall Statistics
# 
# Accuracy : 0.8011         
# 95% CI : (0.7972, 0.805)
# No Information Rate : 0.2183         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.7775         
# Mcnemar's Test P-Value : NA             
# 
# Statistics by Class:
# 
# Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8
# Sensitivity          0.805970 0.663265  0.78312  0.79217  0.75459  0.64056  0.78056  0.85969 0.690619
# Specificity          0.995005 0.989435  0.98897  0.98954  0.99227  0.97540  0.99142  0.99295 0.991826
# Pos Pred Value       0.578158 0.485075  0.72639  0.85559  0.59868  0.65533  0.71745  0.91409 0.518741
# Neg Pred Value       0.998346 0.994919  0.99186  0.98383  0.99623  0.97379  0.99386  0.98782 0.996037
# Prevalence           0.008423 0.014784  0.03605  0.07259  0.01506  0.06806  0.02715  0.08028 0.012596
# Detection Rate       0.006788 0.009805  0.02823  0.05750  0.01136  0.04360  0.02119  0.06901 0.008699
# Detection Prevalence 0.011741 0.020214  0.03887  0.06720  0.01898  0.06653  0.02954  0.07550 0.016770
# Balanced Accuracy    0.900488 0.826350  0.88605  0.89085  0.87343  0.80798  0.88599  0.92632 0.841223
# Class: 9 Class: 10 Class: 11 Class: 12 Class: 13 Class: 14 Class: 15 Class: 16
# Sensitivity            0.8161  0.813187   0.83279   0.86676    0.8932   0.83845  0.658120   0.74968
# Specificity            0.9758  0.996032   0.98970   0.99490    0.9871   0.99505  0.993456   0.97592
# Pos Pred Value         0.9039  0.703422   0.72101   0.76024    0.9338   0.76492  0.472393   0.80417
# Neg Pred Value         0.9500  0.997834   0.99463   0.99751    0.9784   0.99689  0.996945   0.96728
# Prevalence             0.2183  0.011440   0.03098   0.01830    0.1692   0.01883  0.008825   0.11651
# Detection Rate         0.1781  0.009303   0.02580   0.01586    0.1512   0.01579  0.005808   0.08734
# Detection Prevalence   0.1971  0.013225   0.03578   0.02087    0.1619   0.02064  0.012294   0.10861
# Balanced Accuracy      0.8959  0.904610   0.91125   0.93083    0.9401   0.91675  0.825788   0.86280
# Class: 17 Class: 18 Class: 19
# Sensitivity            0.69061   0.81781   0.74047
# Specificity            0.98748   0.99146   0.99181
# Pos Pred Value         0.50556   0.78752   0.61212
# Neg Pred Value         0.99422   0.99294   0.99546
# Prevalence             0.01820   0.03726   0.01715
# Detection Rate         0.01257   0.03047   0.01270
# Detection Prevalence   0.02487   0.03869   0.02074
# Balanced Accuracy      0.83904   0.90464   0.86614

set.seed(292)
fit.xgb <- xgboost(param=parm, data = dtrain, nrounds=min.error.idx)

xgbmodel.predict <- predict(fit.xgb, newdata = dtest)

xgbmodel.predict.text <- levels(traincleaned$cuisine)[xgbmodel.predict + 1]

submission_xgbmodel <- data.frame(ID=test.dat$id, CUISINE=xgbmodel.predict.text)
write.csv(submission_xgbmodel, file=paste0(file.path, "submission_xgbmodel_2.csv"),
          row.names=F)

# Kaggle Score
# Score: 0.80018
# Rank:  200 / 1318 (Top 15%)
# 80.02% accuracy versus maximum achieved 83.22%

save(submission_xgbmodel, file=paste0(saved.obj, "submission-xgbmodel-R-Object"))
rm(submission_xgbmodel)
save(dtrain, file=paste0(saved.obj, "dtrain-new-R-Object"))
rm(dtrain)
save(dtest, file=paste0(saved.obj, "dtest-new-R-Object"))
rm(dtest)
save(fit.xgb, file=paste0(saved.obj, "fit-xgb-new-R-Object"))
rm(fit.xgb)

rm(min.error.idx)
rm(parm)
rm(watchlist)

########## XGBOOST - END ###########################################################


########## RANDOM FOREST - BEGIN ###################################################

# library(h2o)

# According to the 0xdata website, H2O is "The Open Source In-Memory, Prediction Engine 
# for Big Data Science". Indeed, H2O offers an impressive array of machine learning algorithms. 
# The H2O R package provides functions for building GLM, GBM, Kmeans, Naive Bayes, Principal 
# Components Analysis, Principal Components Regression, Random Forests and 
# Deep Learning (multi-layer neural net models).

# After starting H2O, you can use the Web UI at http://localhost:54321
# For more information visit http://docs.h2o.ai

h2o.init()
# R is connected to the H2O cluster: 
#   H2O cluster uptime:         20 seconds 536 milliseconds 
# H2O cluster version:        3.8.1.3 
# H2O cluster name:           H2O_started_from_R_Linesh_yor718 
# H2O cluster total nodes:    1 
# H2O cluster total memory:   0.48 GB 
# H2O cluster total cores:    4 
# H2O cluster allowed cores:  2 
# H2O cluster healthy:        TRUE 
# H2O Connection ip:          localhost 
# H2O Connection port:        54321 
# H2O Connection proxy:       NA 
# R Version:                  R version 3.1.3 (2015-03-09) 
# 
# Note:  As started, H2O is limited to the CRAN default of 2 CPUs.
# Shut down and restart H2O as shown below to use all your CPUs.
# > h2o.shutdown()
# > h2o.init(nthreads = -1)

train.hex <- as.h2o(traincleaned, destination_frame="train.hex")
nrow(train.hex)
# [1] 39775

train.hex <- train.hex[-1,]
nrow(train.hex)
# [1] 39774

test.hex <- as.h2o(testcleaned, destination_frame="test.hex")
nrow(test.hex)
# [1] 9945

test.hex <- test.hex[-1,]
nrow(test.hex)
# [1] 9944

save(train.hex, file=paste0(saved.obj, "train-hex-new-R-Object"))
save(test.hex, file=paste0(saved.obj, "test-hex-new-R-Object"))

y_var <- "cuisine"
x_var <- setdiff(names(train.hex), y_var)

fit.h2o_rf <- h2o.randomForest(x=x_var, y=y_var, training_frame=train.hex,
                               nfolds=5, sample_rate=0.8,
                               ntrees=100)
#                               score_each_iteration = TRUE,
#                               stopping_rounds = 10, stopping_metric="misclassification",
#                               stopping_tolerance = 0.0001,
#                               max_runtime_secs = 18000)

fit.h2o_rf
# Model Details:
# ==============
#   
# H2OMultinomialModel: drf
# Model ID:  DRF_model_R_1470579647797_1 
# Model Summary: 
#   number_of_trees model_size_in_bytes min_depth max_depth mean_depth min_leaves max_leaves mean_leaves
# 1            2000            11730447        20        20   20.00000         74       1075   490.34500
# 
# 
# H2OMultinomialMetrics: drf
# ** Reported on training data. **
#   Description: Metrics reported on Out-Of-Bag training samples
# 
# Training Set Metrics: 
# =====================
# Metrics reported on Out-Of-Bag training samples 
# 
# Extract training frame with `h2o.getFrame("RTMP_sid_81ec_2")`
# MSE: (Extract with `h2o.mse`) 0.3866321
# R^2: (Extract with `h2o.r2`) 0.9851913
# Logloss: (Extract with `h2o.logloss`) 1.130252
# Confusion Matrix: Extract with `h2o.confusionMatrix(<model>,train = TRUE)`)
# =========================================================================
#   Confusion Matrix: vertical: actual; across: predicted
# brazilian british cajun_creole chinese cuisine filipino french greek indian irish italian
# brazilian          189       1            2       1       0        3     10     0     18     0      99
# british              1     202            3       4       0        1     88     4     21    30     164
# cajun_creole         1       2         1047       1       0        1     30     1      7     1     180
# chinese              3       0            3    2336       0        8     10     0     13     1      94
# cuisine              0       0            0       0       0        0      0     0      0     0       0
# jamaican japanese korean mexican moroccan russian southern_us spanish thai vietnamese  Error
# brazilian           2        0      0      83        0       0          43       3   13          0 0.5953
# british             3        0      0       9        0       0         273       0    1          0 0.7488
# cajun_creole        2        0      0      76        1       0         193       3    0          0 0.3228
# chinese             0       35     23      37        1       1          63       0   32         13 0.1261
# cuisine             0        0      0       0        0       0           0       0    0          0       
# Rate
# brazilian    =       278 / 467
# british      =       602 / 804
# cajun_creole =     499 / 1,546
# chinese      =     337 / 2,673
# cuisine      =           0 / 0
# 
# ---
#   brazilian british cajun_creole chinese cuisine filipino french greek indian irish italian
# russian             1       3            3       0       0        0     45     4      6     4     140
# southern_us         3       5          194      12       0        2     74     7     38     5     479
# spanish             2       3           17       1       0        1     51     4     10     1     462
# thai                1       0            0     132       0        5      1     1     62     0      26
# vietnamese          2       1            0     111       0        5      2     1     18     0      51
# Totals            218     255         1313    3166       0      319   1797   718   3250   280   11122
# jamaican japanese korean mexican moroccan russian southern_us spanish thai vietnamese  Error
# russian            1        1      0      18        0     163          98       1    0          1 0.6667
# southern_us        6        5      2     197        0       5        3274       6    6          0 0.2421
# spanish            0        1      0     112        9       1          51     262    0          1 0.7351
# thai               1       14      5      52        1       0          18       0 1178         42 0.2346
# vietnamese         1       12      4      40        0       0          10       0  178        389 0.5285
# Totals           348     1053    578    6925      597     197        5359     315 1504        460 0.2570
# Rate
# russian     =       326 / 489
# southern_us =   1,046 / 4,320
# spanish     =       727 / 989
# thai        =     361 / 1,539
# vietnamese  =       436 / 825
# Totals      = 10,223 / 39,774
# 
# Hit Ratio Table: Extract with `h2o.hit_ratio_table(<model>,train = TRUE)`
# =======================================================================
# Top-10 Hit Ratios: 
#     k hit_ratio
# 1   1  0.742973
# 2   2  0.848217
# 3   3  0.892417
# 4   4  0.920702
# 5   5  0.939558
# 6   6  0.953764
# 7   7  0.963695
# 8   8  0.970986
# 9   9  0.976970
# 10 10  0.981319
# 
# Test Set Metrics: 
# =====================
# Metrics reported on Out-Of-Bag training samples 
# 
# MSE: (Extract with `h2o.mse`) 0.3866321
# R^2: (Extract with `h2o.r2`) 0.9851913
# Logloss: (Extract with `h2o.logloss`) 1.130252
# Confusion Matrix: Extract with `h2o.confusionMatrix(<model>, <data>)`)
# =========================================================================
# Confusion Matrix: vertical: actual; across: predicted
# brazilian british cajun_creole chinese cuisine filipino french greek indian irish italian
# brazilian          189       1            2       1       0        3     10     0     18     0      99
# british              1     202            3       4       0        1     88     4     21    30     164
# cajun_creole         1       2         1047       1       0        1     30     1      7     1     180
# chinese              3       0            3    2336       0        8     10     0     13     1      94
# cuisine              0       0            0       0       0        0      0     0      0     0       0
# jamaican japanese korean mexican moroccan russian southern_us spanish thai vietnamese  Error
# brazilian           2        0      0      83        0       0          43       3   13          0 0.5953
# british             3        0      0       9        0       0         273       0    1          0 0.7488
# cajun_creole        2        0      0      76        1       0         193       3    0          0 0.3228
# chinese             0       35     23      37        1       1          63       0   32         13 0.1261
# cuisine             0        0      0       0        0       0           0       0    0          0       
# Rate
# brazilian    =       278 / 467
# british      =       602 / 804
# cajun_creole =     499 / 1,546
# chinese      =     337 / 2,673
# cuisine      =           0 / 0
# 
# ---
#   brazilian british cajun_creole chinese cuisine filipino french greek indian irish italian
# russian             1       3            3       0       0        0     45     4      6     4     140
# southern_us         3       5          194      12       0        2     74     7     38     5     479
# spanish             2       3           17       1       0        1     51     4     10     1     462
# thai                1       0            0     132       0        5      1     1     62     0      26
# vietnamese          2       1            0     111       0        5      2     1     18     0      51
# Totals            218     255         1313    3166       0      319   1797   718   3250   280   11122
# jamaican japanese korean mexican moroccan russian southern_us spanish thai vietnamese  Error
# russian            1        1      0      18        0     163          98       1    0          1 0.6667
# southern_us        6        5      2     197        0       5        3274       6    6          0 0.2421
# spanish            0        1      0     112        9       1          51     262    0          1 0.7351
# thai               1       14      5      52        1       0          18       0 1178         42 0.2346
# vietnamese         1       12      4      40        0       0          10       0  178        389 0.5285
# Totals           348     1053    578    6925      597     197        5359     315 1504        460 0.2570
# Rate
# russian     =       326 / 489
# southern_us =   1,046 / 4,320
# spanish     =       727 / 989
# thai        =     361 / 1,539
# vietnamese  =       436 / 825
# Totals      = 10,223 / 39,774
# 
# Hit Ratio Table: Extract with `h2o.hit_ratio_table(<model>, <data>)`
# =======================================================================
#   Top-10 Hit Ratios: 
#     k hit_ratio
# 1   1  0.742973
# 2   2  0.848217
# 3   3  0.892417
# 4   4  0.920702
# 5   5  0.939558
# 6   6  0.953764
# 7   7  0.963695
# 8   8  0.970986
# 9   9  0.976970
# 10 10  0.981319
# 
# H2OMultinomialMetrics: drf
# ** Reported on cross-validation data. **
#   Description: 5-fold cross-validation on training data (Metrics computed for combined holdout predictions)
# 
# Cross-Validation Set Metrics: 
# =====================
# 5-fold cross-validation on training data (Metrics computed for combined holdout predictions) 
# 
# Extract cross-validation frame with `h2o.getFrame("RTMP_sid_81ec_2")`
# MSE: (Extract with `h2o.mse`) 0.3860144
# R^2: (Extract with `h2o.r2`) 0.985215
# Logloss: (Extract with `h2o.logloss`) 1.117291
# Hit Ratio Table: Extract with `h2o.hit_ratio_table(<model>,xval = TRUE)`
# =======================================================================
# Top-10 Hit Ratios: 
#     k hit_ratio
# 1   1  0.745562
# 2   2  0.854780
# 3   3  0.900136
# 4   4  0.927767
# 5   5  0.947001
# 6   6  0.959924
# 7   7  0.969176
# 8   8  0.975411
# 9   9  0.980515
# 10 10  0.984512
# 
# Test Set Metrics: 
# =====================
# 5-fold cross-validation on training data (Metrics computed for combined holdout predictions) 
# 
# MSE: (Extract with `h2o.mse`) 0.3860144
# R^2: (Extract with `h2o.r2`) 0.985215
# Logloss: (Extract with `h2o.logloss`) 1.117291
# Confusion Matrix: Extract with `h2o.confusionMatrix(<model>, <data>)`)
# =========================================================================
#   Confusion Matrix: vertical: actual; across: predicted
# brazilian british cajun_creole chinese cuisine filipino french greek indian irish italian
# brazilian          197       0            4       1       0        2      9     0     11     0      95
# british              1     195            3       1       0        0     87     3     24    23     156
# cajun_creole         0       1         1061       3       0        0     29     0      6     1     170
# chinese              2       0            3    2358       0        5      5     0      6     1      90
# cuisine              0       0            0       0       0        0      0     0      0     0       0
# jamaican japanese korean mexican moroccan russian southern_us spanish thai vietnamese  Error
# brazilian           2        0      0      88        0       0          40       3   15          0 0.5782
# british             2        0      0       7        0       2         299       0    1          0 0.7575
# cajun_creole        0        1      0      76        0       0         195       3    0          0 0.3137
# chinese             0       30     23      37        0       0          67       0   36         10 0.1178
# cuisine             0        0      0       0        0       0           0       0    0          0       
# Rate
# brazilian    =       270 / 467
# british      =       609 / 804
# cajun_creole =     485 / 1,546
# chinese      =     315 / 2,673
# cuisine      =           0 / 0
# 
# ---
#   brazilian british cajun_creole chinese cuisine filipino french greek indian irish italian
# russian             0       3            1       0       0        0     45     4      7     4     144
# southern_us         2       5          193      13       0        0     73     5     36     4     456
# spanish             3       0           13       1       0        3     60     4      6     1     474
# thai                0       0            1     134       0        4      1     3     57     0      26
# vietnamese          2       0            0     116       0        5      5     1     10     0      54
# Totals            223     236         1322    3205       0      295   1787   690   3226   260   11104
# jamaican japanese korean mexican moroccan russian southern_us spanish thai vietnamese  Error
# russian            0        0      0      16        1     149         114       1    0          0 0.6953
# southern_us        6        2      0     198        0       5        3318       1    3          0 0.2319
# spanish            0        0      0     113        4       1          56     250    0          0 0.7472
# thai               1       10      6      55        0       0          17       0 1198         26 0.2216
# vietnamese         0        9      4      33        0       0          10       0  193        383 0.5358
# Totals           346     1030    571    6951      586     185        5504     291 1529        433 0.2544
# Rate
# russian     =       340 / 489
# southern_us =   1,002 / 4,320
# spanish     =       739 / 989
# thai        =     341 / 1,539
# vietnamese  =       442 / 825
# Totals      = 10,120 / 39,774
# 
# Hit Ratio Table: Extract with `h2o.hit_ratio_table(<model>, <data>)`
# =======================================================================
# Top-10 Hit Ratios: 
#     k hit_ratio
# 1   1  0.745562
# 2   2  0.854780
# 3   3  0.900136
# 4   4  0.927767
# 5   5  0.947001
# 6   6  0.959924
# 7   7  0.969176
# 8   8  0.975411
# 9   9  0.980515
# 10 10  0.984512

h2o.mse(fit.h2o_rf)
# [1] 0.3866321

predict.h2o_rf <- h2o.predict(fit.h2o_rf, test.hex)

predict.h2o_rf$pred
#    predict
# 1  southern_us
# 2  southern_us
# 3      italian
# 4 cajun_creole
# 5      italian
# 6  southern_us
# 
# [9944 rows x 1 column] 

pred_labels <- as.data.frame(h2o.predict(fit.h2o_rf, test.hex)[,1])

table(pred_labels)
# brazilian   british cajun_creole      chinese     filipino    french        greek 
# 67           66          336          790           73          475          201 
# indian      irish      italian     jamaican     japanese       korean      mexican 
# 827           64         2655           78          238          152         1716 
# moroccan    russian  southern_us      spanish         thai   vietnamese 
# 147           52         1379           80          414          134 

submission_h2o_rf <- data.frame(id=test.dat$id,cuisine=pred_labels)    
nrow(submission_h2o_rf)
# [1] 9944

write.csv(submission_h2o_rf,file=paste0(file.path, "submission_h2o_rf_new.csv"),row.names=F)

# Kaggle Score
# Score: 0.75231
# Rank: 909 / 1318

save(train.hex, file=paste0(saved.obj, file="train-hex-new-R-Object"))
rm(train.hex)
save(test.hex, file=paste0(saved.obj, file="test-hex-new-R-Object"))
rm(test.hex)
save(fit.h2o_rf, file=paste0(saved.obj, "fit-h2o-rf-new-R-Object"))
rm(fit.h2o_rf)

########## RANDOM FOREST - end #####################################################

