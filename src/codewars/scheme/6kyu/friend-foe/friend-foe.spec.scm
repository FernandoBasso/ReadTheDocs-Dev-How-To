(import test)
;;  it('should return the friends', () => {
;;    [
;;      [[], []],
;;      [['K', 'Mu', 'Mia'], []],
;;      [['Goku', 'Lara'], ['Goku', 'Lara']],
;;      [['Ryan', 'Kieran', 'Mark'], ['Ryan', 'Mark']],
;;      [['Ryan', 'Jimmy', '123', '4', 'Cool Man'], ['Ryan']],
;;      [
;;        ['Jimm', 'Cari', 'aret', 'truehdnviegkwgvke', 'sixtyiscooooool'],
;;        ['Jimm', 'Cari', 'aret'],
;;      ],
;;      [['Love', 'Your', 'Face', '😄'], ['Love', 'Your', 'Face']],
;;    ].forEach(([input, output]) => {
;;


(define dataset
  '(
    (() ())
    (('K' 'Mu' 'Mia') ())
    (('Goku' 'Lara') ('Goku' 'Lara'))
    (('Ryan' 'Kieran' 'Mark') ('Ryan' 'Mark'))
    (('Ryan' 'Jimmy' '123' '4' 'Cool Man') ('Ryan'))
    (('Jimm' 'Cari' 'aret' 'truehdnviegkwgvke' 'sixtyiscooooool')
     ('Jimm' 'Cari' 'aret'))
    (('Love' 'Your' 'Face' '😄') ('Love' 'Your' 'Face'))))



