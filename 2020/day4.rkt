#lang racket

(define inp (file->string "day4-input-1"))

(define mandatory-fields
  (list "byr"
        "iyr"
        "eyr"
        "hgt"
        "hcl"
        "ecl"
        "pid"))

(define validators
  (hash
   "byr" (λ(x) (let ([as-num (string->number x)])
                 (and as-num (>= as-num 1920) (<= as-num 2002))))
   "iyr" (λ(x) (let ([as-num (string->number x)])
                 (and as-num (>= as-num 2010) (<= as-num 2020))))
   "eyr" (λ(x) (let ([as-num (string->number x)])
                 (and as-num (>= as-num 2020) (<= as-num 2030))))
   "hgt" (λ(x) (let*-values ([(num-part str-part)
                              (splitf-at (string->list x) char-numeric?)]
                             [(mag) (string->number (list->string num-part))]
                             [(dim) (list->string str-part)])
                 (and mag
                      (or (and (equal? dim "cm") (>= mag 150) (<= mag 193))
                          (and (equal? dim "in") (>= mag 59) (<= mag 76))))))
   "hcl" (λ(x) (regexp-match-exact? #px"#[0-9a-f]{6}" x))
   "ecl" (λ(x) (member x '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))
   "pid" (λ(x) (regexp-match-exact? #px"[0-9]{9}" x))))

(define optional-fields
        (list "cid"))

(define (valid-passport? p)
  (define passport-fields (map first p))
  ;(displayln passport-fields)
  (andmap (λ(x) (member x passport-fields)) mandatory-fields))

(define (valid-passport-2? p)
  (define passport-fields (map first p))
  ;(displayln passport-fields)
  (and (andmap (λ(x) (member x passport-fields)) mandatory-fields)
       (andmap (λ(f) (let ([validator (dict-ref validators f)])
                       (validator (first (dict-ref p f)))))
               mandatory-fields)))

(define (parse-passport s)
  (define segments (string-split s))
  (define categorized (map (λ(x) (string-split x ":")) segments))
  categorized)

(define (parse-passport-file s)
  (map parse-passport (string-split s "\n\n")))


#;(define p
  (parse-passport "ecl:#eef340 eyr:2023 hcl:#c0946f pid:244684338 iyr:2020 cid:57 byr:1969 hgt:152cm"))

#;(valid-passport? p)


(module+ test
  (require rackunit)
  (define test-input #<<HERE
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
HERE
    )
  (check-equal? (count valid-passport? (parse-passport-file test-input)) 2)

  ;; Answers
  (check-equal? (count valid-passport? (parse-passport-file inp)) 206)
  (check-equal? (count valid-passport-2? (parse-passport-file inp)) 123))
