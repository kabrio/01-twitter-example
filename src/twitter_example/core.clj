(ns twitter-example.core
 (:require [clojure.set]
           [clojure.string :as s]
           [clj-http.client :as client]
           [overtone.at-at :as overtone]
           [twitter.api.restful :as twitter]
           [twitter.oauth :as twitter-oauth]
           [environ.core :refer [env]]))

; We are generating tweets based on templates, similar to the game Apples to
; Apples: https://en.wikipedia.org/wiki/Apples_to_Apples
; We start with two lists of strings: One list contains string with blank
; spaces, the other list is used to fill in these spaces.


; We define the "templates" - these strings are the skeleton of the tweet
; We will later replace every occurence of ___ with a string that we chose
; randomly from the list "blanks".
(def templates ["I found a nice ___"
                "The ambassador greets ___ & everyone else travelling"
                "I don't know if ___ will visit us next Millenial"
                "I sent a message to ___"
                "Doing that sort of thing makes you look like ___"])

; Next we define the "blanks"
(def blanks ["beautiful exoplanet"
             "Charles Darwin"
             "rainbow butterfly"
             "Sudoku"
             "Belters"
             "all beings"])

; generate-sentence returns a random sentence, built by choosing one template
; string at random and filling in the blank space (___) with a randomly chosen
; string from the blanks list.
(defn generate-sentence []
  (let [template (rand-nth templates)
        blank (rand-nth blanks)]
      (s/replace template "___" blank)))


(defn alter-word [word]
  (if (> (count word) 1)
    (def wordStart (subs word 0 2))
    (def wordStart word))
  (if (> (count word) 1)
    (def wordEnd (subs word (- (count word) 2) (count word)))
    (def wordEnd ""))
  (def wordEnd2 (s/reverse wordEnd))
  (def newWord (str wordStart wordEnd2))
  newWord)

(defn generate-scramble []
  (let [template (rand-nth templates)
        blank (rand-nth blanks)]
      (def myWords (s/split (s/replace template "___" blank) #" ")))
  (def newSen "")
  (def original "")
  (doseq [word myWords] (def newSen (str newSen (alter-word word) " ")) (def original (str original word " ")))
  ;(pr (str (subs original 0 (- (count original) 1)) "."))
  ;(newline)
  (def finalSentence (str (subs newSen 0 (- (count newSen) 1)) "..."))
  finalSentence)

(defn scramble [sentence]
  (def myWords (s/split sentence #" "))
  (def newSen "")
  (doseq [word myWords] (def newSen (str newSen (alter-word word) " ")))
  (def finalSentence (str (subs newSen 0 (- (count newSen) 1)) "..."))
  finalSentence)

; We retrieve the twitter credentials from the profiles.clj file here.
; In profiles.clj we defined the "env(ironment)" which we use here
; to get the secret passwords we need.
; Make sure you add your credentials in profiles.clj and not here!
(def twitter-credentials (twitter-oauth/make-oauth-creds (env :app-consumer-key)
                                                         (env :app-consumer-secret)
                                                         (env :user-access-token)
                                                         (env :user-access-secret)))


; Tweets are limited to 140 characters. We might randomly generate a sentence
; with more than 140 characters, which would be rejected by Twitter.
; So we check if our generated sentence is longer than 140 characters, and
; don't tweet if so.
(defn tweet [text]
  (when (and (not-empty text) (<= (count text) 140))
   (try (twitter/statuses-update :oauth-creds twitter-credentials
                                 :params {:status text})
        (catch Exception e (println "Something went wrong: " (.getMessage e))))))

; Generate a sentence and tweet it.
(defn tweet-sentence []
  (tweet (generate-sentence)))

(defn tweet-scramble []
  (tweet (generate-scramble)))


(def my-pool (overtone/mk-pool))

(defn -main [& args]
  ;; every 2 hours
  (println "Started up")
  (overtone/every (* 1000 60 60 8) #(println (tweet-scramble)) my-pool))
