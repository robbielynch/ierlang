-module(super_awesome_module).
-export([my_favorite_numbers/0, unlucky_number/0]).

my_favorite_numbers()->
[1, 2, 3, 4, 5].

unlucky_number()->
((278346287 * 98324928374) - (829437 * 3476) - (27368378729560724309 + (2 * 2))).