type t('a);
exception Empty;
let create: unit => t('a);
let push: ('a, t('a)) => unit;
let pop: t('a) => 'a;
let top: t('a) => 'a;
let clear: t('a) => unit;
let copy: t('a) => t('a);
let isEmpty: t('a) => bool;
let length: t('a) => int;
let iter: ('a => unit, t('a));
let fold: (('b, 'a) => 'b, 'b, t('a)) => 'b;