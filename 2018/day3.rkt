#lang racket

#|

adventofcode day 3

data = open("day3-input").read()
lines = data.split('\n')
lines = lines[:-1]
Claim = namedtuple('Claim', ['id', 'topleft', 'topright', 'width', 'height'])
Point = namedtuple('Point', ['x', 'y'])
from itertools import chain
def parse(claim):
    id, rest = claim.split(' @ ')
    id = id[1:]
    tops, rest = rest.split(': ')
    topleft, topright = tops.split(',')
    width, height = rest.split('x')
    return Claim(id, int(topleft), int(topright), int(width), int(height))

def covered_points(c1):
    points = set()
    for x in range(c1.topleft, c1.topleft + c1.width):
        for y in range(c1.topright, c1.topright + c1.height):
            points.add(Point(x, y))
    return points
claims = [parse(x) for x in lines]

def intersecting_points(claims):
    intersected = set()
    covered = set()
    for c in claims:
        this_covers = covered_points(c)
        intersected = intersected.union(covered.intersection(this_covers))
        covered = covered.union(this_covers)
    return intersected, covered

claims = [parse(x) for x in lines]
intersected, covered = intersecting_points(claims)
covered - intersected
len(covered)
len(intersected)
claim_points = covered - intersected
for c in claims:
    if covered_points(c).issubset(claim_points):
        print(c.id)
        break

|#


(define input (file->lines "day3-input"))

(struct Claim (id topleft topright width height) #:transparent)
(struct Point (x y) #:transparent)

(define (parse line)
  (apply
   Claim
   (map
    string->number
    (rest
     (regexp-match
      #rx"#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"
      line)))))

(define parsed-input
  (map parse input))

(define (covered-and-intersecting-points claims)
  (for/fold ([covered (set)]
             [intersecting (set)])
            ([c (in-list claims)])
    (define claimed-points (claim-points c))
    (define new-covered (set-union claimed-points covered))
    (define new-intersecting
      (set-union intersecting (set-intersect claimed-points covered)))
    (values new-covered new-intersecting)))

(define (claim-points claim)
  (list->set
   (for/fold ([points '()])
             ([coords
               (in-list
                (cartesian-product
                 (range
                  (Claim-topleft claim)
                  (+ (Claim-topleft claim) (Claim-width claim)))
                 (range
                  (Claim-topright claim)
                  (+ (Claim-topright claim) (Claim-height claim)))))])
     (cons (Point (first coords) (second coords)) points))))

(define-values (covered intersecting)
  (covered-and-intersecting-points parsed-input))

(set-count intersecting)

(for/last ([c (in-list parsed-input)])
  #:final (set-empty? (set-intersect (claim-points c) intersecting))
  c)
