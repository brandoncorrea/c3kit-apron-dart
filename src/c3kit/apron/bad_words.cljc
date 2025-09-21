(ns c3kit.apron.bad-words
  (:require [clojure.string :as str]))

; region profanity

(def words #{"ahole" "amcik" "andskota" "anus" "arschloch" "ashole" "asholes" "ass" "assface" "assrammer" "asswipe"
             "ayir" "azzhole" "basterds" "basterdz" "biatch" "blow job" "blowjob" "boffing" "boiolas" "boobs" "buceta"
             "butt-pirate" "butthole" "buttwipe" "c0k" "cabron" "carpet muncher" "cawk" "cawks" "cazzo" "chink" "chraa"
             "chuj" "cipa" "clit" "clits" "cnts" "cntz" "cock-head" "cock-sucker" "cum" "cunt" "cunts" "cuntz" "daygo"
             "dego" "dick" "dildo" "dildos" "dirsa" "dominatricks" "dominatrics" "dominatrix" "dupa" "dyke" "dziwka"
             "ejackulate" "ejakulate" "ekto" "enculer" "enema" "f u c k e r" "f u c k" "faen" "faig" "faigs" "fanculo"
             "fart" "fatass" "fcuk" "feces" "feg" "felcher" "ficken" "flikker" "foreskin" "fotze" "fudge packer"
             "futkretzn" "fux0r" "god-damned" "gook" "guiena" "h0ar" "h0r" "h0re" "h4x0r" "hell" "hells" "helvete"
             "hoar" "hoer" "honkey" "hoor" "hoore" "hore" "huevon" "hui" "injun" "jackoff" "jap" "japs" "jerk-off"
             "jisim" "jism" "jiss" "jizm" "jizz" "kawk" "kike" "klootzak" "knobz" "knulle" "kraut" "kuk" "kuksuger"
             "kunt" "kunts" "kuntz" "kurac" "kurwa" "leitch" "lesbo" "lezzian" "lipshits" "lipshitz" "mamhoon"
             "masochist" "masokist" "massterbait" "masstrbait" "masstrbate" "masterbaiter" "masterbate" "masterbates"
             "masturbate" "merd" "mibun" "mofo" "monkleigh" "mouliewop" "muie" "mulkku" "muschi" "nigr" "nastt"
             "nepesaurio" "nigga" "nigger" "nigur" "niiger" "niigr" "nutsack" "orafis" "orgasim" "orgasm" "orgasum"
             "oriface" "orifice" "orifiss" "orospu" "packi" "packie" "packy" "paki" "pakie" "paky" "pecker"
             "peeenus" "peeenusss" "peenus" "peinus" "pen1s" "penas" "penis-breath" "penis" "penus" "penuus" "perse"
             "picka" "pimmel" "pimpis" "pizda" "polac" "polack" "polak" "poonani" "poontsee" "poop" "porn" "pron"
             "pric" "pr1ck" "pr1k" "preteen" "pula" "pule" "pusse" "pussee" "pussy" "puta" "puto" "puuke" "puuker"
             "qahbeh" "rautenberg" "recktum" "rectum" "retard" "s.o.b." "sadist" "scank" "schaffer" "schlampe" "schlong"
             "schmuck" "screw" "screwing" "scrotum" "semen" "sharmuta" "sharmute" "shemale" "shipal" "shit" "shithead"
             "shitty" "shity" "shiz" "shyt" "shyte" "shytty" "shyty" "skanck" "skank" "skankee" "skankey" "skanks"
             "skanky" "skribz" "skurwysyn" "slag" "slut" "sluts" "slutty" "slutz" "smut" "son-of-a-bitch" "sphencter"
             "spic" "spierdalaj" "splooge" "suka" "teets" "teez" "testical" "testicle" "tit" "tits" "titt" "turd"
             "twat" "va1jina" "vag1na" "vagiina" "vagina" "vaj1na" "vajina" "vittu" "vullva" "vulva" "woose" "w00se"
             "w0p" "wank" "wh00r" "wh0re" "whoar" "whore" "wichser" "xrated" "xxx" "yed" "zabourah"})

(def patterns #{"arse*" "asshole*" "bastard*" "boob*" "bitch*" "bollock*" "bullshit*" "cock*" "cunt*" "*damn" "*dyke"
                "dick*" "dike*" "ekrem*" "fag*" "fitt*" "*fuck*" "fu(k*" "fuk*" "hoer*" "kanker*" "kusi*" "kyrpa*"
                "masterbat*" "masturbat*" "merd*" "nazi*" "nigger*" "paska*" "phuc*" "pierdol*" "pillu*" "piss*"
                "queef*" "scheiss*" "sex*" "testicle*" "titt*" "wank*" "wetback*" "wop*"})

(def green-list #{})

; endregion

(defn- escape-except-wildcard [text]
  (str/replace text #"[()\[\]\\.\+\?\^\$\@\{\}\|]" #(str "\\" %)))

(defn- ->l33t [word]
  (-> word
      (str/replace "1" "i")
      (str/replace #"!" "i")
      (str/replace "3" "e")
      (str/replace "4" "a")
      (str/replace "0" "o")
      (str/replace "7" "t")
      (str/replace #"\+" "t")
      (str/replace #"@" "a")))

(defn- matches-swear? [swear text]
  (let [wildcard-swear (-> swear
                           escape-except-wildcard
                           (str/replace "*" ".*")
                           (str/replace " " "\\s*"))
        l33t-text      (->l33t text)
        final-pattern  (str "\\b" wildcard-swear "\\b")
        pattern        (re-pattern final-pattern)]
    (re-find pattern l33t-text)))

(defn- word-pattern [word]
  (let [escaped       (escape-except-wildcard word)
        letter-first? (re-matches #"\w.*" word)
        letter-last?  (re-matches #".*\w$" word)]
    (cond (and letter-first? letter-last?) (str "\\b" escaped "\\b")
          letter-first? (str "\\b" escaped "(?=\\s|$)")
          letter-last? (str "(?:^|\\s)" escaped "\\b")
          :else (str "(?:^|\\s)" escaped "(?=\\s|$)"))))

(defn- extract-word-from-string [string word]
  (str/replace string (re-pattern (str "(?i)\\b" (escape-except-wildcard word) "\\b")) ""))

(defn- remove-green-words [text]
  (reduce extract-word-from-string text green-list))

(defn contains-profanity? [string]
  "Returns true if the submitted string contains profanity"
  (when-let [conformed-text (-> string str/lower-case str/trim ->l33t)]
    (let [no-green (remove-green-words conformed-text)]
      (or (some #(re-find (re-pattern (word-pattern (->l33t %))) no-green) words)
          (some #(matches-swear? % no-green) patterns)))))