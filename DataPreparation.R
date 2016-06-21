# Clear all variables and devices (used for plotting) in the environment
rm(list=ls())
dev.off()

# Load the required packages (if packages are not available, install them first)
for (package in c('jsonlite', 'ggplot2', 'caret', 'tm', 
                  'wordcloud', 'plyr', 'data.table', 'party',
                  'xgboost', 'Matrix')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Declare all variables
file.path <- "C://Users//Linesh//Documents//Linesh//General//DS Meetup//Kaggle//Cooking//Input Files//"
saved.obj <- "C://Users//Linesh//Documents//Linesh//General//DS Meetup//Kaggle//Cooking//R-Objects//"

# Load train and test data sets
test.dat <- fromJSON(paste0(file.path, "test.json"))
train.dat <- fromJSON(paste0(file.path, "train.json"))

save(train.dat, file=paste0(saved.obj, "train-R-Object"))
save(test.dat, file=paste0(saved.obj, "test-R-Object"))

########################## INITIAL EXPLORATION #################################
dev.off()
pie(table(train.dat$cuisine), col=rainbow(20), main="Cuisines Pie Chart")

dev.off()
hist(table(train.dat$cuisine), xlab="No. of Records", main="Cuisines Histogram",
     col=rainbow(8))

#library(ggplot2)
dev.off()
qplot(train.dat$cuisine, xlab="Cuisines", geom="auto")

######################### DATA CLEANING AND EXPLORATION ########################   
dev.off()
# combine both the train and test data sets - useful for clean-up process
combined.data <- rbind(train.dat[,c("id", "ingredients")],test.dat)  
combined.data$ingredients <- lapply(combined.data$ingredients, 
                                    FUN=function(x) gsub("-", " ", x))  

# Create a corpus of ingredient terms
corpus.ingredients <- Corpus(VectorSource(combined.data$ingredients)) 
# corpus ingredients has 49,718 elements, and its size is 210.3 MB
save(combined.data, file=paste0(saved.obj, "combined-data-R-Object"))
rm(combined.data)

# Remove enlish stop words (ex. a, an, the) from the corpus of terms
corpus.cleaned <- tm_map(corpus.ingredients,removeWords,stopwords("english"))   
# corpus.cleaned has 49,718 elements, but its size is 210.2. MB
save(corpus.ingredients, file=paste0(saved.obj, "corpus-ingredients-R-Object"))
rm(corpus.ingredients)
corpus.cleaned[[1]]

# Remove white space from the corpus of terms
corpus.cleaned <- tm_map(corpus.cleaned,stripWhitespace)
# no change in elements or size
corpus.cleaned[[1]]

# Reduce the terms into root form
corpus.cleaned <- tm_map(corpus.cleaned, stemDocument) 
# corpus.cleaned has same number of elements, but the size is now 209.7 MB
corpus.cleaned[[1]]

# Create a Document Term Matrix from the corpus
dtm.ingredients <- DocumentTermMatrix(corpus.cleaned)  
dim(dtm.ingredients) # 49718 x 2785 terms
save(corpus.cleaned, file=paste0(saved.obj, "corpus-cleaned-R-Object"))
rm(corpus.cleaned)

# Retain terms that occur in at least 0.0001 documents or 0.01% of documents
# Amongst 49,718 documents, terms that are in at least 497 documents are 
# retained
dtm.ingredients.spr <- removeSparseTerms(dtm.ingredients, 0.9999) 
dim(dtm.ingredients.spr)  # 49718 x 1739 terms
save(dtm.ingredients, file=paste0(saved.obj, "dtm-ingredients-R-Object"))
rm(dtm.ingredients)

# Inspect the Document Term Matrix
dtm.ingredients.spr
# Output:
# <<DocumentTermMatrix (documents: 49718, terms: 1739)>>
#   Non-/sparse entries: 931964/85527638
# Sparsity           : 99%
# Maximal term length: 13
# Weighting          : term frequency (tf)

# Inspect the first element/row in the Document Term Matrix
dtm.ingredients.spr[[1]]
# [1]   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   2   2   2   2   2

# Inspect the column names of the Document Term Matrix (ingredient terms)
colnames(dtm.ingredients.spr)
# OUTPUT:
#[1] "(10"           "(14.5"         "(powder)"      "33%"           "40%"          
#[6] "95%"           "açai"          "acai"          "accent"        "achiot"       
#[11] "acid"          "acke"          "acorn"         "act"           "activ"        
#[16] "adobo"         "adzuki"        "agar"          "agav"          "age"          
#[21] "ahi"           "aioli"         "ajwain"        "aka"           "albacor"      
#[26] "ale"           "aleppo"        "alfalfa"       "alfredo"       "all"          
#[31] "allspic"       "almond"        "amaretti"      "amaretto"      "amchur"       
#[36] "american"      "amino"         "anaheim"       "ancho"         "anchovi"      
#[41] "andouill"      "anejo"         "angel"         "anglais"       "angostura"    
#[46] "anis"          "anjou"         "annatto"       "appl"          "apple"        
#[51] "applesauc"     "applewood"     "apricot"       "arbol"         "arborio"      
#[56] "armagnac"      "arrowroot"     "artichok"      "artisan"       "arugula"      
#[61] "asada"         "asadero"       "asafetida"     "asafoetida"    "asiago"       
#[66] "asian"         "asparagus"     "assort"        "atta"          "avocado"      
#[71] "babi"          "back"          "bacon"         "bacon,"        "bag"          
#[76] "baguett"       "bailey"        "bake"          "ball"          "balm"         
#[81] "balsam"        "balsamico"     "bamboo"        "banana"        "banger"       
#[86] "bar"           "barbecu"       "barilla"       "barley"        "bartlett"     
#[91] "base"          "basil"         "basmati"       "bass"          "baton"        
#[96] "batter"        "bawang"        "bay"           "bbq"           "bean"         
#[101] "beans,"        "beansprout"    "beaten"        "bechamel"      "beef"         
#[106] "beefsteak"     "beer"          "beet"          "belgian"       "believ"       
#[111] "bell®"         "bell"          "belli"         "ben"           "bengal"       
#[116] "bengali"       "beni"          "berri"         "bertolli®"     "bertolli"     
#[121] "best"          "bianco"        "bibb"          "bicarbon"      "bing"         
#[126] "bird"          "biryani"       "biscotti"      "biscuit"       "bisquick"     
#[131] "bit"           "bitter"        "bittersweet"   "black"         "blackberri"   
#[136] "blacken"       "blade"         "blanc"         "blanch"        "blanco"       
#[141] "blend"         "bliss"         "blood"         "bloodi"        "blossom"      
#[146] "blue"          "blueberri"     "boar"          "bocconcini"    "boil"         
#[151] "bok"           "bologna"       "bone"          "boneless"      "boneless,"    
#[156] "bonito"        "bonnet"        "borlotti"      "bosc"          "boston"       
#[161] "bottl"         "bottom"        "bought"        "bouillon"      "bouquet"      
#[166] "bourbon"       "bow"           "braeburn"      "bragg"         "brais"        
#[171] "bran"          "brand"         "brandi"        "branzino"      "brat"         
#[176] "bread"         "bread,"        "breadcrumb"    "breadstick"    "breakfast"    
#[181] "breast"        "brew"          "brie"          "brine"         "brioch"       
#[186] "brisket"       "broad"         "broccoli"      "broccolini"    "broiler"      
#[191] "broth"         "brown"         "browni"        "brussel"       "bucatini"     
#[196] "buckwheat"     "bud"           "buffalo"       "bulb"          "bulgur"       
#[201] "bulk"          "bun"           "burger"        "burgundi"      "burrata"      
#[206] "butt"          "butter"        "butter!®"      "buttercream"   "butteri"      
#[211] "buttermilk"    "butternut"     "butterscotch"  "button"        "cabbag"       
#[216] "cabernet"      "cacao"         "cachaca"       "cactus"        "caesar"       
#[221] "cajun"         "cake"          "calamansi"     "calamari"      "calamata"     
#[226] "calf"          "california"    "calimyrna"     "callaloo"      "calori"       
#[231] "calvado"       "camembert"     "campanell"     "campari"       "campbel"      
#[236] "can"           "can't"         "canadian"      "candi"         "cane"         
#[241] "canela"        "cannellini"    "cannelloni"    "cannoli"       "canola"       
#[246] "cantaloup"     "canton"        "cap"           "capellini"     "caper"        
#[251] "capers,"       "capicola"      "capsicum"      "caramel"       "caraway"      
#[256] "carbon"        "carcass"       "cardamom"      "cardamon"      "caribbean"    
#[261] "carn"          "carnaroli"     "carnita"       "carrot"        "case"         
#[266] "casera"        "cashew"        "cassava"       "cassi"         "cassia"       
#[271] "caster"        "catalina"      "catfish"       "catsup"        "cauliflow"    
#[276] "cavatappi"     "cavatelli"     "caviar"        "cavolo"        "cayenn"       
#[281] "celeri"        "celtic"        "center"        "cereal"        "chaat"        
#[286] "challa"        "cham"          "chambord"      "champagn"      "chana"        
#[291] "channa"        "chanterell"    "chapati"       "char"          "chard"        
#[296] "chardonnay"    "chat"          "chayot"        "cheddar"       "chee"         
#[301] "chees"         "cheese,"       "cheesi"        "chenpi"        "cherri"       
#[306] "cherryston"    "chervil"       "chestnut"      "chestnuts,"    "chevr"        
#[311] "chèvre"        "chia"          "chianti"       "chicken"       "chickpea"     
#[316] "chiffonad"     "chihuahua"     "chile"         "chilegarl"     "chili"        
#[321] "chilies,"      "chill"         "chilli"        "chines"        "chinkiang"    
#[326] "chip"          "chipotl"       "chive"         "chocol"        "choi"         
#[331] "cholula"       "chop"          "chorizo"       "chow"          "choy"         
#[336] "chrysanthemum" "chuck"         "chunk"         "chunki"        "chutney"      
#[341] "ciabatta"      "cider"         "cilantro"      "cinnamon"      "citric"       
#[346] "citron"        "citrus"        "clam"          "clams,"        "clarifi"      
#[351] "classic"       "classico"      "clear"         "clementin"     "clot"         
#[356] "clove"         "club"          "coar"          "coars"         "cob"          
#[361] "coca"          "cockl"         "cocktail"      "coco"          "cocoa"        
#[366] "coconut"       "cod"           "codfish"       "coffe"         "cognac"       
#[371] "cointreau"     "coke"          "cola"          "colbi"         "cold"         
#[376] "coleslaw"      "collard"       "collect"       "color"         "colour"       
#[381] "comfort"       "compot"        "concentr"      "condens"       "condiment"    
#[386] "confection"    "confit"        "consomm"       "convert"       "cook"         
#[391] "cooki"         "cool"          "coriand"       "corn"          "corn,"        
#[396] "cornbread"     "cornflak"      "cornflour"     "cornhusk"      "cornichon"    
#[401] "cornish"       "cornmeal"      "cotija"        "cottag"        "countri"      
#[406] "couscous"      "cover"         "crab"          "crabmeat"      "crack"        
#[411] "cracker"       "cranberri"     "crawfish"      "cream"         "creami"       
#[416] "crema"         "creme"         "crème"         "cremini"       "creol"        
#[421] "crepe"         "crescent"      "cress"         "crimini"       "crisco"       
#[426] "crisp"         "crispi"        "crock®"        "croissant"     "crookneck"    
#[431] "crostini"      "crouton"       "crumb"         "crumbl"        "crush"        
#[436] "crust"         "crusti"        "crystal"       "cuban"         "cube"         
#[441] "cucumb"        "cuisin"        "cumin"         "cuminse"       "cur"          
#[446] "curaçao"       "curd"          "cure"          "curl"          "currant"      
#[451] "curri"         "custard"       "cut"           "cutlet"        "d'espelett"   
#[456] "daal"          "daikon"        "dairi"         "daisi"         "dal"          
#[461] "dandelion"     "daniel"        "dark"          "dash"          "dashi"        
#[466] "date"          "day"           "decor"         "deep"          "deli"         
#[471] "delici"        "demerara"      "demi"          "dend"          "dessert"      
#[476] "devein"        "devil"         "dhal"          "dice"          "diet"         
#[481] "digest"        "dijon"         "dill"          "dillwe"        "dinner"       
#[486] "dip"           "dish"          "distil"        "ditalini"      "doenzang"     
#[491] "dog"           "dolc"          "domino"        "dorito"        "doubl"        
#[496] "dough"         "drain"         "dress"         "dri"           "drink"        
#[501] "drip"          "drummett"      "drumstick"     "duck"          "dulc"         
#[506] "dumpl"         "dung"          "dust"          "dutch"         "ear"          
#[511] "earl"          "eau"           "edam"          "edamam"        "edibl"        
#[516] "egg"           "eggplant"      "elbow"         "emeril"        "emmenthal"    
#[521] "empanada"      "enchilada"     "endiv"         "english"       "enokitak"     
##[526] "epazot"        "equal"         "escargot"      "escarol"       "espresso"     
#[531] "essenc"        "european"      "evapor"        "extra"         "extract"      
#[536] "eye"           "fajita"        "farina"        "farm"          "farmer"       
#[541] "farro"         "fashion"       "fat"           "fat)"          "fatback"      
#[546] "fatfre"        "fava"          "fed"           "feet"          "fennel"       
#[551] "fenugreek"     "ferment"       "feta"          "fettuccin"     "fettucin"     
#[556] "fideo"         "field"         "fiesta"        "fig"           "file"         
#[561] "filet"         "fill"          "fillet"        "filo"          "fine"         
#[566] "finger"        "fingerl"       "fire"          "firm"          "fish"         
#[571] "fishcak"       "five"          "flake"         "flank"         "flanken"      
#[576] "flat"          "flatbread"     "flavor"        "flax"          "flaxse"       
#[581] "flesh"         "fleur"         "flora"         "floret"        "flounder"     
#[586] "flour"         "flower"        "focaccia"      "foie"          "fontina"      
#[591] "food"          "forest"        "four"          "fraîch"        "frambois"     
#[596] "frangelico"    "frank's®"      "free"          "fregola"       "french"       
#[601] "fresca"        "fresco"        "fresh"         "fresno"        "fri"          
#[606] "frise"         "frito"         "fromag"        "frond"         "frost"        
#[611] "frozen"        "fruit"         "fryer"         "fudg"          "fuji"         
#[616] "full"          "fulli"         "fungus"        "furikak"       "fusilli"      
#[621] "fuyu"          "gaeta"         "gai"           "gala"          "galang"       
#[626] "gallo"         "game"          "garam"         "garbanzo"      "garden"       
#[631] "gari"          "garlic"        "garlic,"       "garni"         "garnish"      
#[636] "gelatin"       "gelato"        "gemelli"       "genoa"         "germ"         
#[641] "ghee"          "gherkin"       "giardiniera"   "giblet"        "gin"          
#[646] "ginger"        "gingerroot"    "gingersnap"    "gizzard"       "glace"        
#[651] "glass"         "glaze"         "globe"         "gluten"        "glutin"       
#[656] "gnocchi"       "goat"          "gochugaru"     "gochujang"     "goji"         
#[661] "gold"          "gold®"         "golden"        "goos"          "gooseberri"   
#[666] "goreng"        "gorgonzola"    "gouda"         "gourd"         "gourmet"      
#[671] "goya"          "grade"         "graham"        "grain"         "gram"         
# [676] "gran"          "grana"         "grand"         "granni"        "granola"      
# [681] "granul"        "grape"         "grapefruit"    "grapese"       "grappa"       
# [686] "gras"          "grass"         "grate"         "gravi"         "greas"        
# [691] "great"         "greater"       "greek"         "green"         "gremolata"    
# [696] "grenadin"      "grey"          "grill"         "grit"          "ground"       
# [701] "groundnut"     "grouper"       "gruyer"        "gruyèr"        "guacamol"     
# [706] "guajillo"      "guancial"      "guava"         "guin"          "gum"          
# [711] "gumbo"         "gyoza"         "haa"           "habanero"      "haddock"      
# [716] "hair"          "half"          "halibut"       "halv"          "ham"          
# [721] "hamburg"       "hand"          "hanger"        "hanh"          "hanout"       
# [726] "hard"          "haricot"       "harina"        "harissa"       "hash"         
# [731] "hass"          "hatch"         "havarti"       "hazelnut"      "head"         
# [736] "heart"         "heavi"         "heinz"         "heirloom"      "hellmann's®"  
# [741] "hellmann"      "hemp"          "hen"           "herb"          "herdez"       
# [746] "herring"       "hibiscus"      "hidden"        "high"          "himalayan"    
# [751] "hoagi"         "hock"          "hog"           "hoisin"        "holi"         
# [756] "hollandais"    "home"          "homemad"       "homestyl"      "homini"       
# [761] "honey"         "honeydew"      "horseradish"   "hot"           "hothous"      
# [766] "hsing"         "hummus"        "hungarian"     "husk"          "ice"          
# [771] "iceberg"       "idaho"         "imit"          "imo"           "indian"       
# [776] "instant"       "irish"         "iron"          "island"        "italian"      
# [781] "jack"          "jackfruit"     "jaggeri"       "jalap"         "jalapeno"     
# [786] "jam"           "jamaican"      "jambalaya"     "japanes"       "jarlsberg"    
# [791] "jasmin"        "jeera"         "jell"          "jelli"         "jerk"         
# [796] "jerki"         "jerusalem"     "jicama"        "johnsonvill"   "juic"         
# [801] "jumbo"         "junip"         "jus"           "kabocha"       "kaffir"       
# [806] "kahlua"        "kahlúa"        "kaiser"        "kalamata"      "kale"         
# [811] "kalonji"       "karo"          "kasha"         "kasseri"       "kasuri"       
# [816] "kecap"         "kefalotyri"    "kefir"         "kelp"          "kernel"       
# [821] "ketchup"       "kewpi"         "kewra"         "key"           "khoa"         
# [826] "kidnei"        "kidney"        "kielbasa"      "kikkoman"      "kim"          
# [831] "kimchi"        "king"          "kirbi"         "kirsch"        "kitchen"      
# [836] "kiwi"          "knockwurst"    "knorr"         "knorr®"        "kochujang"    
# [841] "kombu"         "konbu"         "konnyaku"      "korean"        "kosher"       
# [846] "kraft"         "kumquat"       "lacinato"      "ladi"          "ladyfing"     
# [851] "lager"         "lamb"          "lan"           "lard"          "lardon"       
# [856] "larg"          "lasagn"        "lasagna"       "laurel"        "lavend"       
# [861] "lea"           "leaf"          "leafi"         "lean"          "leav"         
# [866] "leaves,"       "lech"          "leek"          "leg"           "lemon"        
# [871] "lemonad"       "lemongrass"    "lentil"        "less"          "lettuc"       
# [876] "light"         "lili"          "lima"          "lime"          "limead"       
# [881] "limoncello"    "linguin"       "linguini"      "link"          "lipton®"      
# [886] "lipton"        "liqueur"       "liquid"        "liquor"        "lite"         
# [891] "littleneck"    "liver"         "loaf"          "loav"          "lobster"      
# [896] "loin"          "long"          "loos"          "lotus"         "louisiana"    
# [901] "low"           "lower"         "lowfat"        "lowsodium"     "luke"         
# [906] "lump"          "lumpia"        "lyche"         "macadamia"     "macaroni"     
# [911] "mace"          "machin"        "mackerel"      "madeira"       "madra"        
# [916] "maggi"         "mahi"          "mahimahi"      "maida"         "maldon"       
# [921] "malt"          "maltos"        "mam"           "manchego"      "mandarin"     
# [926] "mango"         "mani"          "manicotti"     "manioc"        "manzanilla"   
# [931] "mapl"          "margarin"      "margarita"     "mari"          "marin"        
# [936] "marinad"       "marinara"      "marjoram"      "marmalad"      "marmit"       
# [941] "marnier"       "marrow"        "marsala"       "marshmallow"   "marzano"      
# [946] "marzipan"      "masa"          "masala"        "mascarpon"     "mash"         
# [951] "masoor"        "massaman"      "matcha"        "matur"         "matzo"        
# [956] "mayer"         "mayonais"      "mayonnai"      "mayonnais"     "mazola"       
# [961] "mccormick"     "meal"          "meat"          "meatbal"       "meatloaf"     
# [966] "medal"         "medium"        "medjool"       "mein"          "melon"        
# [971] "melt"          "merguez"       "meringu"       "merlot"        "merluza"      
# [976] "mesclun"       "methi"         "mex"           "mexican"       "mexicana"     
# [981] "mexicorn"      "meyer"         "mezcal"        "mignon"        "mild"         
# [986] "milk"          "millet"        "min"           "minc"          "mincemeat"    
# [991] "mini"          "miniatur"      "minicub"       "mint"          "minut"        
# [996] "miracl"        "mirin"         "mirliton"      "miso"          "mission"      
# [1001] "mitsuba"       "mix"           "mochiko"       "moistur"       "molass"       
# [1006] "mole"          "monkfish"      "monterey"      "moong"         "morel"        
# [1011] "morsel"        "mortadella"    "mountain"      "mozzarella"    "mrs."         
# [1016] "msg"           "muenster"      "muffin"        "mulato"        "mullet"       
# [1021] "mung"          "muscovado"     "mushroom"      "mussel"        "mussels,"     
# [1026] "mustard"       "mutton"        "naan"          "nacho"         "nam"          
# [1031] "napa"          "natur"         "navel"         "navi"          "neck"         
# [1036] "nectar"        "nectarin"      "negro"         "nero"          "neufchâtel"   
# [1041] "neutral"       "new"           "niçois"        "nigella"       "nilla"        
# [1046] "noir"          "non"           "nonfat"        "nonstick"      "noodl"        
# [1051] "noodles,"      "nopal"         "nopalito"      "nori"          "northern"     
# [1056] "not"           "nuoc"          "nut"           "nutella"       "nutmeg"       
# [1061] "nutrit"        "oat"           "oatmeal"       "octopus"       "oil"          
# [1066] "okra"          "old"           "olek"          "oliv"          "olive"        
# [1071] "oloroso"       "onion"         "orang"         "orecchiett"    "oregano"      
# [1076] "oreo®"         "organ"         "origin"        "orzo"          "oscar"        
# [1081] "ouzo"          "oven"          "oxtail"        "oyster"        "oz.)"         
# [1086] "pace"          "pack"          "padano"        "pak"           "palm"         
# [1091] "pan"           "pancak"        "pancetta"      "pancit"        "pandan"       
# [1096] "pandanus"      "paneer"        "panela"        "panetton"      "panko"        
# [1101] "papaya"        "paper"         "pappadam"      "pappardell"    "paprika"      
# [1106] "parboil"       "parma"         "parmagiano"    "parmesan"      "parmigiana"   
# [1111] "parmigiano"    "parsley"       "parsnip"       "part"          "pasilla"      
# [1116] "paso"          "pasoT"         "passata"       "passion"       "past"         
# [1121] "pasta"         "pasta,"        "pastri"        "pate"          "pati"         
# [1126] "pattypan"      "pea"           "peach"         "peanut"        "pear"         
# [1131] "pearl"         "peasant"       "pecan"         "pecorino"      "pectin"       
# [1136] "peel"          "penn"          "peperoncini"   "peperoncino"   "pepita"       
# [1141] "pepper"        "pepper,"       "peppercorn"    "pepperidg"     "peppermint"   
# [1146] "pepperoncini"  "pepperoni"     "peppers,"      "perciatelli"   "perilla"      
# [1151] "pernod"        "perrin"        "persian"       "persimmon"     "pesto"        
# [1156] "petal"         "petit"         "philadelphia"  "phyllo"        "picant"       
# [1161] "picholin"      "pickl"         "pico"          "pie"           "piec"         
# [1166] "piecrust"      "pierogi"       "pig"           "pigeon"        "pilaf"        
# [1171] "pillsburyT"    "piloncillo"    "piment"        "pimento"       "pimenton"     
# [1176] "pine"          "pineappl"      "pinenut"       "pink"          "pinot"        
# [1181] "pinto"         "pistachio"     "pit"           "pita"          "pizza"        
# [1186] "pla"           "plain"         "plantain"      "plum"          "poblano"      
# [1191] "pocket"        "pod"           "poha"          "point"         "polenta"      
# [1196] "polish"        "pomegran"      "pomelo"        "pompeian"      "ponzu"        
# [1201] "popcorn"       "poppi"         "porcini"       "pork"          "porridg"      
# [1206] "port"          "portabello"    "porterhous"    "portobello"    "posol"        
# [1211] "pot"           "potato"        "poultri"       "pound"         "powder"       
# [1216] "pralin"        "prawn"         "prebak"        "prego"         "premium"      
# [1221] "prepar"        "preserv"       "press"         "process"       "progresso"    
# [1226] "promis"        "prosciutto"    "prosecco"      "protein"       "provenc"      
# [1231] "provolon"      "prune"         "pud"           "puf"           "puff"         
# [1236] "pulp"          "pumpernickel"  "pumpkin"       "pumpkinse"     "puré"         
# [1241] "pure"          "purpl"         "purpo"         "purpos"        "puy"          
# [1246] "quail"         "quarter"       "queso"         "quick"         "quickcook"    
# [1251] "quinc"         "quinoa"        "rabbit"        "rabe"          "rack"         
# [1256] "radicchio"     "radish"        "ragu"          "rainbow"       "rais"         
# [1261] "raisin"        "raita"         "rajma"         "ramen"         "ramp"         
# [1266] "ranch"         "ranch®"        "rang"          "rapese"        "rapid"        
# [1271] "ras"           "rasher"        "raspberri"     "ravioli"       "ravva"        
# [1276] "raw"           "readi"         "real"          "recip"         "red"          
# [1281] "redhot®"       "reduc"         "reduct"        "refri"         "refriger"     
# [1286] "reggiano"      "regular"       "relish"        "remoulad"      "render"       
# [1291] "rhubarb"       "rib"           "rice"          "rich"          "ricotta"      
# [1296] "riesl"         "rigat"         "rigatoni"      "rin"           "rind"         
# [1301] "ring"          "rioja"         "ripe"          "ripen"         "rise"         
# [1306] "risotto"       "ritz"          "roast"         "rock"          "rocket"       
# [1311] "roe"           "roll"          "roma"          "romain"        "romano"       
# [1316] "rome"          "root"          "roquefort"     "rosé"          "rose"         
# [1321] "rosemari"      "rosewat"       "rotel"         "rotell"        "rotini"       
# [1326] "rotisseri"     "rouill"        "round"         "roux"          "rub"          
# [1331] "rubi"          "rum"           "rump"          "runni"         "russet"       
# [1336] "russian"       "rutabaga"      "rye"           "safflow"       "saffron"      
# [1341] "sage"          "sago"          "saigon"        "sake"          "salad"        
# [1346] "salami"        "salata"        "salmon"        "salsa"         "salt"         
# [1351] "saltin"        "sambal"        "sambuca"       "san"           "sand"         
# [1356] "sandwich"      "sansho"        "santo"         "sardin"        "sargento®"    
# [1361] "satsuma"       "sauc"          "sauerkraut"    "sausag"        "sautern"      
# [1366] "sauvignon"     "savori"        "savoy"         "sazon"         "scallion"     
# [1371] "scallop"       "schmaltz"      "schnapp"       "scotch"        "scrub"        
# [1376] "sea"           "seafood"       "season"        "seawe"         "sec"          
# [1381] "seca"          "seed"          "seedless"      "segment"       "seitan"       
# [1386] "sel"           "self"          "seltzer"       "semi"          "semisweet"    
# [1391] "semolina"      "serrano"       "sesam"         "shahi"         "shallot"      
# [1396] "shanghai"      "shank"         "shao"          "shaox"         "sharp"        
# [1401] "shave"         "sheep'"        "sheet"         "shell"         "shellfish"    
# [1406] "sherri"        "shichimi"      "shiitak"       "shimeji"       "shirataki"    
# [1411] "shiro"         "shiso"         "shoepeg"       "shoga"         "shoot"        
# [1416] "short"         "shortbread"    "shortcrust"    "shorten"       "shoulder"     
# [1421] "shoyu"         "shred"         "shrimp"        "shrimp,"       "shuck"        
# [1426] "sichuan"       "sicilian"      "sidesT"        "side"          "silken"       
# [1431] "silver"        "simpl"         "singl"         "sirloin"       "siu"          
# [1436] "size"          "skate"         "skim"          "skin"          "skinless"     
# [1441] "skirt"         "slab"          "slaw"          "slice"         "slider"       
# [1446] "sliver"        "small"         "smith"         "smoke"         "smooth"       
# [1451] "snail"         "snapper"       "snow"          "soak"          "soba"         
# [1456] "soda"          "sodium"        "sofrito"       "soft"          "soften"       
# [1461] "sole"          "solid"         "somen"         "sooji"         "soppressata"  
# [1466] "sorbet"        "sorghum"       "sorrel"        "soup"          "sour"         
# [1471] "sourdough"     "southern"      "soy"           "soya"          "soybean"      
# [1476] "soymilk"       "spaghetti"     "spaghetti,"    "spaghettini"   "spam"         
# [1481] "spanish"       "spare"         "sparerib"      "sparkl"        "spear"        
# [1486] "spearmint"     "spelt"         "spice"         "spici"         "spike"        
# [1491] "spinach"       "spinach,"      "spiral"        "splenda"       "split"        
# [1496] "spong"         "spray"         "spread"        "sprig"         "spring"       
# [1501] "spring!"       "sprinkl"       "sprite"        "sprout"        "squash"       
# [1506] "squeez"        "squid"         "squirt"        "sriracha"      "star"         
# [1511] "starch"        "starchi"       "steak"         "steam"         "steamer"      
# [1516] "steel"         "stem"          "stevia"        "stew"          "stick"        
# [1521] "sticki"        "stilton"       "stir"          "stock"         "stone"        
# [1526] "stonefir"      "store"         "stout"         "straw"         "strawberri"   
# [1531] "streaki"       "string"        "strip"         "stripe"        "strong"       
# [1536] "stuf"          "style"         "sub"           "substitut"     "suet"         
# [1541] "sugar"         "sugarcan"      "sultana"       "sum"           "sumac"        
# [1546] "summer"        "sun"           "sunda"         "sundri"        "sunflow"      
# [1551] "superfin"      "sushi"         "swanson"       "sweet"         "sweeten"      
# [1556] "swiss"         "swordfish"     "syd"           "syrup"         "szechwan"     
# [1561] "tabasco"       "tabl"          "taco"          "tagliatell"    "tahini"       
# [1566] "tail"          "tamal"         "tamari"        "tamarind"      "tandoori"     
# [1571] "tangerin"      "tapenad"       "tapioca"       "taro"          "tarragon"     
# [1576] "tart"          "tartar"        "tasso"         "tater"         "tatsoi"       
# [1581] "tawni"         "tea"           "tempeh"        "tender"        "tenderloin"   
# [1586] "tentacl"       "tequila"       "teriyaki"      "tex"           "texa"         
# [1591] "thai"          "thaw"          "thick"         "thigh"         "thin"         
# [1596] "thousand"      "thread"        "thyme"         "tie"           "tiger"        
# [1601] "tilapia"       "tip"           "toast"         "toffe"         "tofu"         
# [1606] "togarashi"     "tom"           "tomatillo"     "tomato"        "tomatoes,"    
# [1611] "ton"           "tongu"         "tonkatsu"      "toor"          "top"          
# [1616] "topping,"      "tortellini"    "tortilla"      "tostada"       "tot"          
# [1621] "tradit"        "treacl"        "tri"           "tripe"         "tripl"        
# [1626] "trout"         "truffl"        "tubetti"       "tumer"         "tuna"         
# [1631] "turbinado"     "turkei"        "turkey"        "turkish"       "turmer"       
# [1636] "turnip"        "turtl"         "twist"         "tzatziki"      "udon"         
# [1641] "ulek"          "ume"           "umeboshi"      "unbak"         "unbleach"     
# [1646] "uncl"          "uncle"         "uncook"        "undrain"       "unflavor"     
# [1651] "unsalt"        "unseason"      "unsweeten"     "urad"          "valley®"      
# [1656] "vanilla"       "varnish"       "vay®"          "veal"          "vegan"        
# [1661] "veget"         "vegetables,"   "vegetarian"    "veggi"         "velveeta"     
# [1666] "venison"       "verbena"       "verd"          "vermicelli"    "vermouth"     
# [1671] "vert"          "vidalia"       "vie"           "vietnames"     "vin"          
# [1676] "vinaigrett"    "vine"          "vinegar"       "vineyard"      "virgin"       
# [1681] "vodka"         "wafer"         "waffl"         "wakam"         "walnut"       
# [1686] "warm"          "wasabi"        "water"         "watercress"    "watermelon"   
# [1691] "wax"           "waxi"          "wedg"          "weed"          "well"         
# [1696] "wheat"         "wheel"         "whip"          "whiskey"       "whiski"       
# [1701] "white"         "whitefish"     "whole"         "wholem"        "wide"         
# [1706] "wild"          "wine"          "wing"          "winter"        "wish"         
# [1711] "won"           "wondra"        "wonton"        "wood"          "worcestershir"
# [1716] "world"         "wrap"          "wrapper"       "xanthan"       "yakisoba"     
# [1721] "yam"           "yardlong"      "yeast"         "yellow"        "yellowfin"    
# [1726] "yoghurt"       "yogurt"        "yolk"          "york"          "young"        
# [1731] "yucca"         "yukon"         "yum"           "yuzu"          "zest"         
# [1736] "zesti"         "zinfandel"     "ziti"          "zucchini"     

# Find terms that are repeated at least 15000 times
findFreqTerms(dtm.ingredients.spr, 15000) # 10 terms
# OUTPUT:
# [1] "fresh"  "garlic" "ground" "oil"    "oliv"   "onion"  "pepper" "salt"   
#     "sauc"   "sugar"

# Find terms that are repeated at least 10000 times
findFreqTerms(dtm.ingredients.spr, 10000) # 24 terms
# OUTPUT:
# [1] "black"   "butter"  "chees"   "chicken" "chop"    "clove"   "dri"     "egg"
#     "flour"  
# [10] "fresh"   "garlic"  "green"   "ground"  "oil"     "oliv"    "onion"   "pepper"
#      "powder" 
# [19] "red"     "salt"    "sauc"    "sugar"   "tomato"  "water"  

# Create a Simple Matrix from the Sparse Document Term Matrix
dtm.mat <- as.matrix(dtm.ingredients.spr)

# get sum of rows for each term
dtm.mat.sum  <- colSums(dtm.mat)

# get the length of the columns of this matrix 
length(dtm.mat.sum)
# OUTPUT:
# [1] 1739

# Inspect the Simple Matrix
dtm.mat.sum
# OUTPUT:
# (10         (14.5      (powder)           33%           40%           95% 
#              5             5             7             8            14             6 
#              açai          acai        accent        achiot          acid          acke 
#              25             6             8            32             7            12 
#              acorn           act         activ         adobo        adzuki          agar 
#              21            24           489           397             6            11 
#              agav           age           ahi         aioli        ajwain           aka 
#              106            25            11            11            18             5 
#              albacor           ale        aleppo       alfalfa       alfredo           all 
#              12            37            15             5            91            16 
#              allspic        almond      amaretti      amaretto        amchur      american 
#              682          1452            21            43           108            56 
#              amino       anaheim         ancho       anchovi      andouill         anejo 
#              43            66           308           413           358             7 
#              angel       anglais     angostura          anis         anjou       annatto 
#              129             9            14           495            11            25 
#              appl         apple     applesauc     applewood       apricot         arbol 
#              1040            12            32            20           308            60 
#              arborio      armagnac     arrowroot      artichok       artisan       arugula 
#              363            19            43           364             5           283 
#              asada       asadero     asafetida    asafoetida        asiago         asian 
#              5            31            66           134           104           316 
#              asparagus        assort          atta       avocado          babi          back 
#              419             7            10          1614           969            67 
#              bacon        bacon,           bag       baguett        bailey          bake 
#              1527             5            92           420            12          3791 
#              ball          balm        balsam     balsamico        bamboo        banana 
#              20             6           735             7           174           392 
#              banger           bar       barbecu       barilla        barley      bartlett 
#              7            33           176            10            70            23 
#              base         basil       basmati          bass         baton        batter 
#              263          3837           424            92             9            21 
#              bawang           bay           bbq          bean        beans,    beansprout 
#              30          2690            33          4994            26           585 
#              beaten      bechamel          beef     beefsteak          beer          beet 
#              70             8          3801            29           380           170 
#              belgian        believ         bell®          bell         belli           ben 
#              49            26             7          5336           159            12 
#              bengal       bengali          beni         berri     bertolli®      bertolli 
#              18             8             6           163            34            17 
#              best        bianco          bibb      bicarbon          bing          bird 
#              65             7            31            19             5            87 
#              biryani      biscotti       biscuit      bisquick           bit        bitter 
#              15             6           157            23            25            45 
#              bittersweet         black    blackberri       blacken         blade         blanc 
#              190         13473           124             6             5            18 
#              blanch        blanco         blend         bliss         blood        bloodi 
#              131            18           311             6            26             5 
#              blossom          blue     blueberri          boar    bocconcini          boil 
#              36           101           122             6            17           783 
#              bok       bologna          bone      boneless     boneless,        bonito 
#              252             5           305          3410            42           116 
#              bonnet      borlotti          bosc        boston         bottl        bottom 
#              54             7            27            75            77            16 
#              bought      bouillon       bouquet       bourbon           bow      braeburn 
#              64           308            30           265           140             6 
#              bragg         brais          bran         brand        brandi      branzino 
#              7             7            22             7           241             8 
#              brat         bread        bread,    breadcrumb    breadstick     breakfast 
#              9          2843            13           260            15            47 
#              breast          brew          brie         brine        brioch       brisket 
#              4159            92            47            76            22            89 
#              broad      broccoli    broccolini       broiler         broth         brown 
#              10           653            27            20          5572          3775 
#              browni       brussel      bucatini     buckwheat           bud       buffalo 
#              10            47            18            56            12            25 
#              bulb        bulgur          bulk           bun        burger      burgundi 
#              284            40            28           156            17            35 
#              burrata          butt        butter      butter!®   buttercream       butteri 
#              9           161         10816            24             7            10 
#              buttermilk     butternut  butterscotch        button        cabbag      cabernet 
#              1266           221             7           220          1549            12 
#              cacao       cachaca        cactus        caesar         cajun          cake 
#              12            95            10            16           491           443 
#              calamansi      calamari      calamata          calf    california     calimyrna 
#              40            23             9             6            79            18 
#              callaloo        calori       calvado     camembert     campanell       campari 
#              12            10            25             5             7            12 
#              campbel           can         can't      canadian         candi          cane 
#              27           440            24            27           159            79 
#              canela    cannellini    cannelloni       cannoli        canola     cantaloup 
#              22           256             5             5          1634            30 
#              canton           cap     capellini         caper       capers,      capicola 
#              7           134            14           677             5             7 
#              capsicum       caramel       caraway        carbon       carcass      cardamom 
#              85            54           111            23            12           949 
#              cardamon     caribbean          carn     carnaroli       carnita        carrot 
#              21             6            13            16            13          4061 
#              case        casera        cashew       cassava         cassi        cassia 
#              150             5           448            13            11            15 
#              caster      catalina       catfish        catsup     cauliflow     cavatappi 
#              200             8           155             9           394            12 
#              cavatelli        caviar        cavolo        cayenn        celeri        celtic 
#              11            28             5          2678          2701             9 
#              center        cereal         chaat        challa          cham      chambord 
#              72            37            50             7            12             9 
#              champagn         chana        channa    chanterell       chapati          char 
#              69            27            13            21             9            19 
#              chard    chardonnay          chat        chayot       cheddar          chee 
#              158            13            21            41          2331           164 
#              chees       cheese,        cheesi        chenpi        cherri    cherryston 
#              14536           186             7            10           819             5 
#              chervil      chestnut    chestnuts,         chevr        chèvre          chia 
#              40           298            10            22            14            13 
#              chianti       chicken      chickpea     chiffonad     chihuahua         chile 
#              12         14449           588             9            14          3707 
#              chilegarl         chili      chilies,         chill        chilli        chines 
#              16          8915             9             5           182           901 
#              chinkiang          chip       chipotl         chive        chocol          choi 
#              27           709           720           773          1088            17 
#              cholula          chop       chorizo          chow          choy chrysanthemum 
#              5         10244           322            37           271             7 
#              chuck         chunk        chunki       chutney      ciabatta         cider 
#              303            68           184           141            59           833 
#              cilantro      cinnamon        citric        citron        citrus          clam 
#              7432          3263             6            16            14           416 
#              clams,       clarifi       classic      classico         clear     clementin 
#              8            69            10            23            29             7 
#              clot         clove          club          coar         coars           cob 
#              6         10217            58             6           903            15 
#              coca         cockl      cocktail          coco         cocoa       coconut 
#              14            10            68             9           479          2767 
#              cod       codfish         coffe        cognac     cointreau          coke 
#              139            29           305           117            35             8 
#              cola         colbi          cold      coleslaw       collard       collect 
#              30            93           673            45           222             6 
#              color        colour       comfort        compot      concentr       condens 
#              143            44            14             6           151           678 
#              condiment    confection        confit       consomm       convert          cook 
#              14           502            21            19            24          4895 
#              cooki          cool       coriand          corn         corn,     cornbread 
#              95            15          2740          6605           133            69 
#              cornflak     cornflour      cornhusk     cornichon       cornish      cornmeal 
#              32           129             8            40            34           630 
#              cotija        cottag       countri      couscous         cover          crab 
#              171           225           211           265             6           331 
#              crabmeat         crack       cracker     cranberri      crawfish         cream 
#              104           509           185           159           145          7708 
#              creami         crema         creme         crème       cremini         creol 
#              151           115            31           203           170           447 
#              crepe      crescent         cress       crimini        crisco         crisp 
#              21            57             5            82            15            13 
#              crispi        crock®     croissant     crookneck      crostini       crouton 
#              9            24            13            12            10            59 
#              crumb        crumbl         crush         crust        crusti       crystal 
#              1242           627          2546           323            62            59 
#              cuban          cube        cucumb        cuisin         cumin       cuminse 
#              15           352          1475            16          5813             5 
#              cur       curaçao          curd          cure          curl       currant 
#              30             5           152           108            10           187 
#              curri       custard           cut        cutlet    d'espelett          daal 
#              2046            20           198           131             8            13 
#              daikon         dairi         daisi           dal     dandelion        daniel 
#              192            10             9           217            12             8 
#              dark          dash         dashi          date           day         decor 
#              1343             6           197           171             5            13 
#              deep          deli        delici      demerara          demi          dend 
#              10            46            50            11            29             8 
#              dessert        devein         devil          dhal          dice          diet 
#              8            99             5            11          2891             5 
#              digest         dijon          dill        dillwe        dinner           dip 
#              5           711           559            43            34            99 
#              dish        distil      ditalini      doenzang           dog          dolc 
#              7            24            33             7            43             7 
#              domino        dorito         doubl         dough         drain         dress 
#              5            16           100           575           322           525 
#              dri         drink          drip      drummett     drumstick          duck 
#              10518             9           125            11           159           178 
#              dulc         dumpl          dung          dust         dutch           ear 
#              9            82            14            54            26           103 
#              earl           eau          edam        edamam         edibl           egg 
#              6             7             7           104            12         11348 
#              eggplant         elbow        emeril     emmenthal      empanada     enchilada 
#              757           116             8            11            12           516 
#              endiv       english      enokitak        epazot         equal      escargot 
#              64           294            58            51            15             5 
#              escarol      espresso        essenc      european        evapor         extra 
#              76           154            57             9           310          3869 
#              extract           eye        fajita        farina          farm        farmer 
#              2203           329            49             5            16            19 
#              farro       fashion           fat          fat)       fatback        fatfre 
#  27            26          3251             6            15            15 
#  fava           fed          feet        fennel     fenugreek       ferment 
#  53             5            16           821           222            47 
#  feta     fettuccin      fettucin         fideo         field        fiesta 
#  825            71           189            10            22            10 
#  fig          file         filet          fill        fillet          filo 
#  120            81            66            46          1394             9 
#  fine        finger       fingerl          fire          firm          fish 
#  1144            63            28            21           667          2313 
#  fishcak          five         flake         flank       flanken          flat 
#  12           358          2502           321             9          1391 
#  flatbread        flavor          flax        flaxse         flesh         fleur 
#  37           205            18            10            12            35 
#  flora        floret      flounder         flour        flower      focaccia 
#  19           401            21         11104            90            15 
#  foie       fontina          food        forest          four        fraîch 
#  16           175           264            12            16           189 
#  frambois    frangelico      frank's®          free       fregola        french 
#  6            15             6          1283             5           383 
#  fresca        fresco         fresh        fresno           fri         frise 
#  14           229         23482            18           142            22 
#  frito        fromag         frond         frost        frozen         fruit 
#  12             7            44            52          1991           168 
#  fryer          fudg          fuji          full         fulli        fungus 
#  42            10            11            41            22             9 
#  furikak       fusilli          fuyu         gaeta           gai          gala 
#  10            65             6             7            48            14 
#  galang         gallo          game         garam      garbanzo        garden 
#  107           107            15          1183           185            31 
#  gari        garlic       garlic,         garni       garnish       gelatin 
#  57         23597            27            23            11           181 
#  gelato       gemelli         genoa          germ          ghee       gherkin 
#  5            18            35            19           403            17 
#  giardiniera        giblet           gin        ginger    gingerroot    gingersnap 
#  12            15            25          6735           143            16 
#  gizzard         glace         glass         glaze         globe        gluten 
#  8            39            54            43             5           116 
#  glutin       gnocchi          goat     gochugaru     gochujang          goji 
#  70            75           311            36           178            12 
#  gold         gold®        golden          goos    gooseberri        goreng 
#  311             7           520             9             6            30 
#  gorgonzola         gouda         gourd       gourmet          goya         grade 
#  84            41             8             8            11             9 
#  graham         grain          gram          gran         grana         grand 
#  95          1057            74            19            11            39 
#  granni       granola        granul         grape    grapefruit       grapese 
#  146            17          1303           412            73            94 
#  grappa          gras         grass         grate         gravi         greas 
#  14            16           139          4239            69            32 
#  great       greater         greek         green     gremolata      grenadin 
#  61             6           629         11374             6            18 
#  grey         grill          grit        ground     groundnut       grouper 
#  7            20           386         22846            12            20 
#  gruyer        gruyèr      guacamol      guajillo      guancial         guava 
#  135            77           242           119            27            11 
#  guin           gum         gumbo         gyoza           haa      habanero 
#  34            21            24            45            10            85 
#  haddock          hair          half       halibut          halv           ham 
#  34           119           909           127          1240           848 
#  hamburg          hand        hanger          hanh        hanout          hard 
#  94            18            10             7            53           168 
#  haricot        harina       harissa          hash          hass         hatch 
#  60           122            82            28            54             9 
#  havarti      hazelnut          head         heart         heavi         heinz 
#  11           189            23           317          1744             8 
#  heirloom   hellmann's®      hellmann          hemp           hen          herb 
#  42             8            60             8            46           345 
#  herdez       herring      hibiscus        hidden          high     himalayan 
#  14             5            10            15             5            11 
#  hoagi          hock           hog        hoisin          holi    hollandais 
#  26           137             9           519             7             5 
#  home       homemad      homestyl        homini         honey      honeydew 
#  6            75             5           188          1757            22 
#  horseradish           hot       hothous         hsing        hummus     hungarian 
#  164          2380            54             9            26            83 
#  husk           ice       iceberg         idaho          imit           imo 
#  112           767           217            19            27             9 
#  indian       instant         irish          iron        island       italian 
#  6           335            92            11            12          1712 
#  jack     jackfruit       jaggeri         jalap      jalapeno           jam 
#  1204            17            49            18          2223           124 
#  jamaican     jambalaya       japanes     jarlsberg        jasmin         jeera 
#  52            10           173             6           207            54 
#  jell         jelli          jerk         jerki     jerusalem        jicama 
#  11            56            71             5             7           110 
#  johnsonvill          juic         jumbo         junip           jus       kabocha 
#  25          8974           138            30             7            11 
#  kaffir        kahlua        kahlúa        kaiser      kalamata          kale 
#  175             6            23            13           514           247 
#  kalonji          karo         kasha       kasseri        kasuri         kecap 
#  6             5             5             5            67            23 
#  kefalotyri         kefir          kelp        kernel       ketchup         kewpi 
#  9             5            36           813           645             7 
#  kewra           key          khoa        kidnei        kidney      kielbasa 
#  12            49             9            26           373            63 
#  kikkoman           kim        kimchi          king         kirbi        kirsch 
#  12             5           154            25            31            22 
#  kitchen          kiwi    knockwurst         knorr        knorr®     kochujang 
#  5            43             5            66            14             8 
#  kombu         konbu      konnyaku        korean        kosher         kraft 
#  19            88             5            48          4015            56 
#  kumquat      lacinato          ladi      ladyfing         lager          lamb 
#  22            11             9            52            38           726 
#  lan          lard        lardon          larg        lasagn       lasagna 
#  47           209             8          6787            25           357 
#  laurel        lavend           lea          leaf         leafi          lean 
#  5            25             5          2601             9           699 
#  leav       leaves,          lech          leek           leg         lemon 
#  6930            27             9           565           354          7289 
#  lemonad    lemongrass        lentil          less        lettuc         light 
#  33           450           434           683          1525          1823 
#  lili          lima          lime        limead    limoncello       linguin 
#  18            88          6203            26             6           312 
#  linguini          link       lipton®        lipton       liqueur        liquid 
#  12           107             6             8           277           156 
#  liquor          lite    littleneck         liver          loaf          loav 
#  9            25            76           148            42            19 
#  lobster          loin          long          loos         lotus     louisiana 
#  87           342           875            40            18            27 
#  low         lower        lowfat     lowsodium          luke          lump 
#  2836           174            19            15             9           141 
#  lumpia         lyche     macadamia      macaroni          mace        machin 
#  32             8            15           155           106             6 
#  mackerel       madeira         madra         maggi          mahi      mahimahi 
#  14            41            45            22            44             9 
#  maida        maldon          malt        maltos           mam      manchego 
#  20             8            66             9             6            78 
#  mandarin         mango          mani     manicotti        manioc    manzanilla 
#  50           438            23            38            14             9 
#  mapl      margarin     margarita          mari         marin       marinad 
#  178           331            10             5            66           114 
#  marinara      marjoram      marmalad        marmit       marnier        marrow 
#  290           197            61            13            58            13 
#  marsala   marshmallow       marzano      marzipan          masa        masala 
#  148            78             6            10           174          1358 
#  mascarpon          mash        masoor      massaman        matcha         matur 
#  180            87            12            11            17             6 
#  matzo         mayer      mayonais      mayonnai     mayonnais        mazola 
#  16            10           983            59           219             8 
#  mccormick          meal          meat       meatbal      meatloaf         medal 
#  20           493           801            47            10             9 
#  medium       medjool          mein         melon          melt       merguez 
#  774            28            72            45           343            11 
#  meringu        merlot       merluza       mesclun         methi           mex 
#  21             9             5            13            95             5 
#  mexican      mexicana      mexicorn         meyer        mezcal        mignon 
#  650            25            28            27             5            19 
#  mild          milk        millet           min          minc     mincemeat 
#  156          7356            13            16          2639            16 
#  mini      miniatur       minicub          mint         minut        miracl 
#  89            12            17          1701            14             7 
#  mirin      mirliton          miso       mission       mitsuba           mix 
#  626            13           345            21             9          1107 
#  mochiko       moistur        molass          mole      monkfish      monterey 
#  9             7           178            23            13           732 
#  moong         morel        morsel    mortadella      mountain    mozzarella 
#  38            16           121            11             5          1765 
#  mrs.           msg      muenster        muffin        mulato        mullet 
#  6            33            11            53            13             6 
#  mung     muscovado      mushroom        mussel      mussels,       mustard 
#  160            31          2990           228             6          2253 
#  mutton          naan         nacho           nam          napa         natur 
#  29            50             8            22           291            79 
#  navel          navi          neck        nectar      nectarin         negro 
#  69            37            19           141            29             5 
#  nero    neufchâtel       neutral           new        niçois       nigella 
#  5             7            47           106            30            30 
#  nilla          noir           non        nonfat      nonstick         noodl 
#  12            11           192           319            60          1716 
#  noodles,         nopal      nopalito          nori      northern           not 
#  45            28             5           182            61            24 
#  nuoc           nut       nutella        nutmeg        nutrit           oat 
#  25           475            12          1358            34           139 
#  oatmeal       octopus           oil          okra           old          olek 
#  17            18         29102           415           261             6 
#  oliv         olive       oloroso         onion         orang    orecchiett 
#  15242            34             5         24065          2308            57 
#  oregano         oreo®         organ        origin          orzo         oscar 
#  3439             9           115            48           152            10 
#  ouzo          oven        oxtail        oyster          oz.)          pace 
# 12            56            43           727            39            24 
# pack        padano           pak          palm           pan        pancak 
# 295            10            17           204            19            32 
# pancetta        pancit        pandan      pandanus        paneer        panela 
# 216             5             6            23           160             6 
# panetton         panko        papaya         paper      pappadam    pappardell 
# 7           265            78            99             5            30 
# paprika       parboil         parma    parmagiano      parmesan    parmigiana 
# 2411            14             5            17          3659             5 
# parmigiano       parsley       parsnip          part       pasilla          paso 
# 516          5176            87           485            50            23 
# pasoT       passata       passion          past         pasta        pasta, 
# 46            30            19          3702          1528            10 
# pastri          pate          pati      pattypan           pea         peach 
# 442            12            11             5          1841           463 
# peanut          pear         pearl       peasant         pecan      pecorino 
# 2188           269           168            12           755           275 
# pectin          peel          penn   peperoncini   peperoncino        pepita 
# 11          1610           397            15             6            32 
# pepper       pepper,    peppercorn     pepperidg    peppermint  pepperoncini 
# 33879            86          1038             9             8            27 
# pepperoni      peppers,   perciatelli       perilla        pernod        perrin 
# 125            62            10            19            28             5 
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
# pit          pita         pizza           pla         plain      plantain 
# 491           203           431            14          1121            50 
# plum       poblano        pocket           pod          poha         point 
# 1215           394             6           353            15             7 
# polenta        polish      pomegran        pomelo      pompeian         ponzu 
# 194            10            73             5             7            29 
# popcorn         poppi       porcini          pork       porridg          port 
# 14           100           139          3077             5            81 
# portabello    porterhous    portobello         posol           pot        potato 
# 86             7            28             9            17          3672 
# poultri         pound        powder        pralin         prawn        prebak 
# 78            29         10987             6           173            18 
# prego       premium        prepar       preserv         press       process 
# 10             6           254           194             9            82 
# progresso        promis    prosciutto      prosecco       protein       provenc 
# 12             6           353            16             5            80 
# provolon         prune           pud           puf          puff          pulp 
# 199           106            78             5           216            25 
# pumpernickel       pumpkin     pumpkinse          puré          pure         purpl 
# 9           246            13           294           310          2376 
# purpo        purpos           puy         quail       quarter         queso 
# 7          6059             9            27            35           276 
# quick     quickcook         quinc        quinoa        rabbit          rabe 
# 78           111            17           189            27            66 
# rack     radicchio        radish          ragu       rainbow          rais 
# 21            82           403            47             5            10 
# raisin         raita         rajma         ramen          ramp         ranch 
# 891            10             9            93            16           127 
# ranch®          rang        rapese         rapid           ras        rasher 
# 10            64            18            20            53             8 
# raspberri       ravioli         ravva           raw         readi          real 
# 296            63            15           129            61            55 
# recip           red       redhot®         reduc        reduct         refri 
# 14         11538             6          1191             6           358 
# refriger      reggiano       regular        relish      remoulad        render 
# 225           535            56            71             5            14 
# rhubarb           rib          rice          rich       ricotta         riesl 
# 31          1211          7681             8           903            14 
# rigat      rigatoni           rin          rind          ring         rioja 
# 22           108            32           239            15             6 
# ripe         ripen          rise       risotto          ritz         roast 
# 122            49           317            28             7          1659 
# rock        rocket           roe          roll          roma        romain 
# 72            58             8           504           275           429 
# romano          rome          root     roquefort          rosé          rose 
# 365             6           711            16            12            56 
# rosemari       rosewat         rotel        rotell        rotini     rotisseri 
# 1034             5            37            43            80            86 
# rouill         round          roux           rub          rubi           rum 
# 6           219            13           108            17           302 
# rump         runni        russet       russian      rutabaga           rye 
# 30            17           340             6            22            40 
# safflow       saffron          sage          sago        saigon          sake 
# 54           610           563             9             6           427 
# salad        salami        salata        salmon         salsa          salt 
# 433           119            46           388          1692         30836 
# saltin        sambal       sambuca           san          sand      sandwich 
# 55            79             5             6            10           116 
# sansho         santo        sardin     sargento®       satsuma          sauc 
# 6            11            26             8            16         16490 
# sauerkraut        sausag       sautern     sauvignon        savori         savoy 
# 41          1998             6            19            28            69 
# sazon      scallion       scallop      schmaltz       schnapp        scotch 
# 14          2443           256             8            14            70 
# scrub           sea       seafood        season         seawe           sec 
# 14          1857            65          3161            73            48 
# seca          seed      seedless       segment        seitan           sel 
# 5          5126           126            14             6            35 
# self       seltzer          semi     semisweet      semolina       serrano 
# 302            22           101           225           153           687 
# sesam         shahi       shallot      shanghai         shank          shao 
# 4392            23          1866             8           140             9 
# shaox         sharp         shave        sheep'         sheet         shell 
# 330           595            34             5           184           551 
# shellfish        sherri      shichimi       shiitak       shimeji     shirataki 
# 5           780            22           741             7            15 
# shiro         shiso       shoepeg         shoga         shoot         short 
# 15            33             8             7           196           238 
# shortbread    shortcrust       shorten      shoulder         shoyu         shred 
# 7             7           445           525            29          3333 
# shrimp       shrimp,         shuck       sichuan      sicilian        sidesT 
# 2911            24            46            20            11             8 
# side        silken        silver         simpl         singl       sirloin 
# 8            72            25            50            21           354 
# siu          size         skate          skim          skin      skinless 
# 13            22             6           684            87          2801 
# skirt          slab          slaw         slice        slider        sliver 
# 88            14           102          2415             5           234 
# small         smith         smoke        smooth         snail       snapper 
# 219           146          1078             5            20           106 
# snow          soak          soba          soda        sodium       sofrito 
# 223             5           135          1318          2800             5 
# soft        soften          sole         solid         somen         sooji 
# 204           244            20            26             9             5 
# soppressata        sorbet       sorghum        sorrel          soup          sour 
# 17            12            18            17           601          2486 
# sourdough      southern           soy          soya       soybean       soymilk 
# 105            14          6090            17            48             9 
# spaghetti    spaghetti,   spaghettini          spam       spanish         spare 
# 540            10            33             7           190            17 
# sparerib        sparkl         spear     spearmint         spelt         spice 
# 48            15            77            10            18           747 
# spici         spike       spinach      spinach,        spiral       splenda 
# 60            14          1694            25             5            37 
# split         spong         spray        spread         sprig        spring 
# 82            19          2513            83           988           606 
# spring!       sprinkl        sprite        sprout        squash        squeez 
# 7            75            14           214           562            25 
# squid        squirt      sriracha          star        starch       starchi 
# 115             8           380           326          2414             6 
# steak         steam       steamer         steel          stem        stevia 
# 1157           123            10            10           108            26 
# stew         stick        sticki       stilton          stir         stock 
# 420           959            39            34            56          2406 
# stone      stonefir         store         stout         straw    strawberri 
# 34             5            64            34            39           311 
# streaki        string         strip        stripe        strong          stuf 
# 12            27            47             9            18           118 
# style           sub     substitut          suet         sugar      sugarcan 
# 447            11           102            27         15650             6 
# sultana           sum         sumac        summer           sun         sunda 
# 25            14             8            42           262             5 
# sundri       sunflow      superfin         sushi       swanson         sweet 
# 20           143            93           111            24          2073 
# sweeten         swiss     swordfish           syd         syrup      szechwan 
# 542           281            33             5           762           178 
# tabasco          tabl          taco    tagliatell        tahini          tail 
# 258           103           812            26            81            22 
# tamal        tamari      tamarind      tandoori      tangerin       tapenad 
# 8           140           260            43            41            20 
# tapioca          taro      tarragon          tart        tartar         tasso 
# 136            22           309            68           228            33 
# tater        tatsoi         tawni           tea        tempeh        tender 
# 11             5            23           286            18            50 
# tenderloin       tentacl       tequila      teriyaki           tex          texa 
# 467            15           200            81             5             6 
# thai          thaw         thick         thigh          thin      thousand 
# 906            46            88          1047            49             5 
# thread         thyme           tie         tiger       tilapia           tip 
# 399          3082           133            35           120            59 
# toast         toffe          tofu     togarashi           tom     tomatillo 
# 1117            18           794            37             6           416 
# tomato     tomatoes,           ton         tongu      tonkatsu          toor 
# 13944             5            45            18            15            17 
# top      topping,    tortellini      tortilla       tostada           tot 
# 273             8            93          3327            51            11 
# tradit        treacl           tri         tripe         tripl         trout 
# 20            12            14             9            48            46 
# truffl       tubetti         tumer          tuna     turbinado        turkei 
# 74             9           963           247            64            12 
# turkey       turkish        turmer        turnip         turtl         twist 
# 778            22          1218           197             6             9 
# tzatziki          udon          ulek           ume      umeboshi         unbak 
# 20            89            63             5             8            28 
# unbleach          uncl         uncle        uncook       undrain      unflavor 
# 228             7             8            82            14           132 
# unsalt      unseason     unsweeten          urad       valley®       vanilla 
# 3673             5           683            84            15          2782 
# varnish          vay®          veal         vegan         veget   vegetables, 
# 15             5           239            50          7433             7 
# vegetarian         veggi      velveeta       venison       verbena          verd 
# 20            38            38            11             6           176 
# vermicelli      vermouth          vert       vidalia           vie     vietnames 
# 217            87            59           140             7            85 
# vin    vinaigrett          vine       vinegar      vineyard        virgin 
# 11           103            67          6190             6          3451 
# vodka         wafer         waffl         wakam        walnut          warm 
# 101            87             7            21           420           752 
# wasabi         water    watercress    watermelon           wax          waxi 
# 112         12226           140            58             7            39 
# wedg          weed          well         wheat         wheel          whip 
# 900            25            14           782            13          1360 
# whiskey        whiski         white     whitefish         whole        wholem 
# 357             6          9819            68          2582             6 
# wide          wild          wine          wing        winter          wish 
# 38            96          5335           178            15            26 
# won        wondra        wonton          wood worcestershir         world 
# 45             7           206            44           879            27 
# wrap       wrapper       xanthan      yakisoba           yam      yardlong 
# 14           463            21            12            75            28 
# yeast        yellow     yellowfin       yoghurt        yogurt          yolk 
# 961          2864             5           346          1482          1423 
# york         young         yucca         yukon           yum          yuzu 
# 24             6             5           273             6            11 
# zest         zesti     zinfandel          ziti      zucchini 
# 1129            26            13            51          1152 

save(dtm.mat, file=paste0(saved.obj, "dtm-mat-R-Object"))
rm(dtm.mat)

# For every term (column), get the number of rows/recipes, the term is part of
mat.ord <- order(dtm.mat.sum)

# Inspect the column totals
mat.ord

#check least and most frequent terms
dtm.mat.sum[head(mat.ord)]  # this gives the least used terms
dtm.mat.sum[tail(mat.ord)]  # this gives the most used terms

#load(file=paste0(saved.obj, "dtm-mat-R-Object"))

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

#check our table of 20 frequencies
#load(file=paste0(saved.obj, "mat-ord-R-Object"))
head(table(mat.ord),20)
tail(table(mat.ord),20)


#find associated terms
findAssocs(dtm.ingredients.spr, c('salt','oil'), corlimit=0.30)
# OUTPUT:
# $salt
# numeric(0)
#
# $oil
# oliv  sesam  veget garlic 
# 0.47   0.35   0.34   0.31 

# Jumbled Chart
dev.off()
rm(corpus.cleaned)
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
term # list words to compare the recipe terms

dtm.mat[35067,colnames(dtm.mat) %in% c("black","pepper","chicken","onion","red")]

rm(dtm.mat)

#create a data frame for visualization for frequency of ingredients
wf <- data.frame(word = names(dtm.mat.sum), freq = dtm.mat.sum)
head(wf)

#plot terms which appear atleast 15,000 times
#library(ggplot2)
#library(plyr)
#library(data.table)
dev.off()
## set the levels in order we want
wf.sub <- data.table(subset(wf, freq >15000))
wf.sub
# word  freq
# 1:  fresh 23482
# 2: garlic 23597
# 3: ground 22846
# 4:    oil 29102
# 5:   oliv 15242
# 6:  onion 24065
# 7: pepper 33879
# 8:   salt 30836
# 9:   sauc 16490
# 10:  sugar 15650

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

#plot word cloud of terms having a minumum frequency of 200
wordcloud(names(dtm.mat.sum), dtm.mat.sum, min.freq = 200, scale = c(2, .1), 
          colors = brewer.pal(4, "BuPu"))

#plot 300 most used words
dev.off()
set.seed(143)
wordcloud(names(dtm.mat.sum), dtm.mat.sum, max.words = 300, scale = c(3, .1), 
          colors = brewer.pal(6, 'Dark2'))

dev.off()

save(dtm.mat.sum, file=paste0(saved.obj, "dtm-mat-sum-R-Object"))
save(mat.ord, file=paste0(saved.obj, "mat-ord-R-Object"))
save(lda, file=paste0(saved.obj, "lda-R-Object"))
rm(dtm.mat.sum)
rm(mat.ord)
rm(lda)

