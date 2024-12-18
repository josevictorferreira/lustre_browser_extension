 #   L u s t r e   B r o w s e r   E x t e n s i o n 
 
 A   m o d e r n   b o i l e r p l a t e   f o r   c r e a t i n g   c r o s s - b r o w s e r   e x t e n s i o n s   u s i n g   [ G l e a m ] ( h t t p s : / / g l e a m . r u n )   a n d   [ L u s t r e ] ( h t t p s : / / l u s t r e . b u i l d )   w i t h   [ V i t e ] ( h t t p s : / / v i t e j s . d e v ) .   B u i l d   e x t e n s i o n s   t h a t   w o r k   s e a m l e s s l y   o n   b o t h   C h r o m e   a n d   F i r e f o x   u s i n g   a   s i n g l e   c o d e b a s e .   T h i s   p r o j e c t   i s   b a s e d   o n   [ V i t e s s e   W e b E x t ] ( h t t p s : / / g i t h u b . c o m / a n t f u / v i t e s s e - w e b e x t ) . 
 
 # #   F e a t u r e s 
 
 -  �=ހ   C r o s s - b r o w s e r   s u p p o r t   f o r   C h r o m e   a n d   F i r e f o x 
 -  &��   V i t e - p o w e r e d   b u i l d   s y s t e m 
 -  �<�   [ G l e a m ] ( h t t p s : / / g l e a m . r u n )   a n d   [ L u s t r e ] ( h t t p s : / / l u s t r e . b u i l d )   f o r   t y p e - s a f e ,   f u n c t i o n a l   p r o g r a m m i n g 
 -  �<ߨ   [ T a i l w i n d   C S S ] ( h t t p s : / / t a i l w i n d c s s . c o m )   f o r   s t y l i n g 
 -  �=�%   H o t   M o d u l e   R e p l a c e m e n t   ( H M R ) 
 -  �=��   O p t i m i z e d   b u i l d s 
 -  �>��   E x t e n s i o n   m a n i f e s t   v 3   s u p p o r t 
 
 # #   G e t t i n g   S t a r t e d 
 
 # # #   P r e r e q u i s i t e s 
 
 -   [ N o d e . j s ] ( h t t p s : / / n o d e j s . o r g / )   ( v 1 4   o r   h i g h e r ) 
 -   [ G l e a m ] ( h t t p s : / / g l e a m . r u n / g e t t i n g - s t a r t e d / i n s t a l l i n g / ) 
 -   [ p n p m ] ( h t t p s : / / p n p m . i o / i n s t a l l a t i o n )   ( r e c o m m e n d e d )   o r   n p m 
 
 # # #   I n s t a l l a t i o n 
 
 1 .   C l o n e   t h i s   r e p o s i t o r y : 
 ` ` ` b a s h 
 g i t   c l o n e   h t t p s : / / g i t h u b . c o m / j o s e v i c t o r f e r r e i r a / l u s t r e - b r o w s e r - e x t e n s i o n 
 c d   l u s t r e - b r o w s e r - e x t e n s i o n 
 ` ` ` 
 
 2 .   C o p y   a n d   e d i t   y o u r   . e n v   w i t h   t h e   p r o j e c t   d e f a u l t s : 
 ` ` ` b a s h 
 c p   . e x a m p l e . e n v   . e n v 
 ` ` ` 
 
 3 .   I n s t a l l   d e p e n d e n c i e s : 
 ` ` ` b a s h 
 p n p m   i n s t a l l 
 g l e a m   i n s t a l l 
 ` ` ` 
 
 4 .   S t a r t   d e v e l o p m e n t   s e r v e r : 
 ` ` ` b a s h 
 p n p m   r u n   d e v 
 ` ` ` 
 
 # # #   D e v e l o p m e n t 
 
 T h e   b o i l e r p l a t e   s u p p o r t s   d i f f e r e n t   d e v e l o p m e n t   m o d e s : 
 
 ` ` ` b a s h 
 #   D e v e l o p m e n t   m o d e   w i t h   H M R   f o r   C h r o m e 
 p n p m   r u n   d e v 
 
 #   D e v e l o p m e n t   m o d e   w i t h   H M R   f o r   F i r e f o x 
 p n p m   r u n   d e v - f i r e f o x 
 
 #   B u i l d   e x t e n s i o n   f o r   p r o d u c t i o n 
 p n p m   b u i l d 
 
 #   B u i l d   f o r   C h r o m e 
 p n p m   b u i l d 
 
 #   B u i l d   f o r   F i r e f o x 
 p n p m   b u i l d - f i r e f o x 
 ` ` ` 
 
 # #   L o a d i n g   t h e   E x t e n s i o n 
 
 # # #   C h r o m e 
 
 1 .   G o   t o   ` c h r o m e : / / e x t e n s i o n s / ` 
 2 .   E n a b l e   " D e v e l o p e r   m o d e " 
 3 .   C l i c k   " L o a d   u n p a c k e d " 
 4 .   S e l e c t   t h e   ` e x t e n s i o n `   d i r e c t o r y 
 
 # # #   F i r e f o x 
 
 1 .   G o   t o   ` a b o u t : d e b u g g i n g # / r u n t i m e / t h i s - f i r e f o x ` 
 2 .   C l i c k   " L o a d   T e m p o r a r y   A d d - o n " 
 3 .   S e l e c t   a n y   f i l e   i n   t h e   ` e x t e n s i o n `   d i r e c t o r y 
 
 # #   C u s t o m i z a t i o n 
 
 # # #   M a n i f e s t   C o n f i g u r a t i o n 
 
 E d i t   ` s r c / l i b / m a n i f e s t . g l e a m `   t o   c u s t o m i z e   y o u r   e x t e n s i o n ' s   m a n i f e s t : 
 
 # # #   S t y l i n g 
 
 T h i s   b o i l e r p l a t e   u s e s   T a i l w i n d   C S S   f o r   s t y l i n g .   C o n f i g u r e   T a i l w i n d   i n   ` t a i l w i n d . c o n f i g . j s ` : 
 
 ` ` ` j a v a s c r i p t 
 m o d u l e . e x p o r t s   =   { 
     c o n t e n t :   [ " . / s r c / * * / * . { g l e a m , h t m l } " ] , 
     / /   . . .   o t h e r   c o n f i g u r a t i o n 
 } 
 ` ` ` 
 
 # #   C r e d i t s 
 
 T h i s   p r o j e c t   i s   b a s e d   o n   [ v i t e s s e - w e b e x t ] ( h t t p s : / / g i t h u b . c o m / a n t f u / v i t e s s e - w e b e x t ) ,   a d a p t e d   f o r   u s e   w i t h   G l e a m   a n d   L u s t r e . 
 
 # #   A c k n o w l e d g m e n t s 
 
 -   [ V i t e s s e   W e b E x t ] ( h t t p s : / / g i t h u b . c o m / a n t f u / v i t e s s e - w e b e x t ) 
 -   [ G l e a m ] ( h t t p s : / / g l e a m . r u n ) 
 -   [ L u s t r e ] ( h t t p s : / / l u s t r e . b u i l d ) 
 -   [ V i t e ] ( h t t p s : / / v i t e j s . d e v ) 
 -   [ T a i l w i n d   C S S ] ( h t t p s : / / t a i l w i n d c s s . c o m ) 
