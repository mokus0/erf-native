-- GSL.SpecFunc.Erf - translated from specfunc/erfc.c in GSL sources.
-- Original header from specfunc/erfc.c follows.

-- specfunc/erfc.c
-- 
-- Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 Gerard Jungman
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3 of the License, or (at
-- your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
module GSL.SpecFunc.Erf (erf, erfc, erff, erfcf) where

import Math.Polynomial.Chebyshev

-- I'm lazy: these should be implemented as native Float operations
-- with truncated series, etc., but they aren't.
erff :: Float -> Float
erff = realToFrac . erf . realToFrac
erfcf :: Float -> Float
erfcf = realToFrac . erfc . realToFrac


erf :: Double -> Double
erf x
    | abs x < 1 = erfseries x
    | otherwise = 1 - erfc x

erfseries :: Double -> Double
erfseries x = go 1 x x
    where
        go k coef e
            | k < 30    = 
                let
                    k'      = k + 1
                    coef'   = coef * negate (x*x/k)
                    e'      =  e + coef' / (2 * k + 1)
                in go k' coef' e'
            | otherwise = 2 / sqrt pi * e
 
erfc :: Double -> Double
erfc x 
    | x >= 0    = estimate
    | otherwise = 2 - estimate
    where
        ax = abs x
        estimate
            | ax <= 1   = evalChebyshevSeries erfc_xlt1_cs (2 * ax - 1)
            | ax <= 5   = 
                let t = 0.5 * (ax - 3)
                in exp (negate x*x) * evalChebyshevSeries erfc_x15_cs t
            | ax < 10   = 
                let t = (2 * ax - 15) / 5
                in (exp (negate x*x) / ax) * evalChebyshevSeries erfc_x510_cs t
            | otherwise = erfc8 ax

erfc8 :: Double -> Double
erfc8 x = erfc8_sum x * exp (negate x * x)

-- |estimates erfc(x) valid for 8 < x < 100
-- This is based on index 5725 in Hart et al
erfc8_sum :: Double -> Double
erfc8_sum x = num / den
    where
        p = [ 2.97886562639399288862
            , 7.409740605964741794425
            , 6.1602098531096305440906
            , 5.019049726784267463450058
            , 1.275366644729965952479585264
            , 0.5641895835477550741253201704
            ]
        q = [ 3.3690752069827527677
            , 9.608965327192787870698
            , 17.08144074746600431571095
            , 12.0489519278551290360340491
            , 9.396034016235054150430579648
            , 2.260528520767326969591866945
            , 1.0
            ]
        num = poly p x
        den = poly q x

poly p x = go p
    where
        go []       = 0
        go (c:cs)   = c + x * go cs 

erfc_xlt1_cs =
    [   1.06073416421769980345174155056 / 2
    , -0.42582445804381043569204735291
    ,  0.04955262679620434040357683080
    ,  0.00449293488768382749558001242
    , -0.00129194104658496953494224761
    , -0.00001836389292149396270416979
    ,  0.00002211114704099526291538556
    , -5.23337485234257134673693179020e-7
    , -2.78184788833537885382530989578e-7
    ,  1.41158092748813114560316684249e-8
    ,  2.72571296330561699984539141865e-9
    , -2.06343904872070629406401492476e-10
    , -2.14273991996785367924201401812e-11
    ,  2.22990255539358204580285098119e-12
    ,  1.36250074650698280575807934155e-13
    , -1.95144010922293091898995913038e-14
    , -6.85627169231704599442806370690e-16
    ,  1.44506492869699938239521607493e-16
    ,  2.45935306460536488037576200030e-18
    , -9.29599561220523396007359328540e-19
    ]
erfc_x15_cs =
    [  0.44045832024338111077637466616 / 2
    , -0.143958836762168335790826895326
    ,  0.044786499817939267247056666937
    , -0.013343124200271211203618353102
    ,  0.003824682739750469767692372556
    , -0.001058699227195126547306482530
    ,  0.000283859419210073742736310108
    , -0.000073906170662206760483959432
    ,  0.000018725312521489179015872934
    , -4.62530981164919445131297264430e-6
    ,  1.11558657244432857487884006422e-6
    , -2.63098662650834130067808832725e-7
    ,  6.07462122724551777372119408710e-8
    , -1.37460865539865444777251011793e-8
    ,  3.05157051905475145520096717210e-9
    , -6.65174789720310713757307724790e-10
    ,  1.42483346273207784489792999706e-10
    , -3.00141127395323902092018744545e-11
    ,  6.22171792645348091472914001250e-12
    , -1.26994639225668496876152836555e-12
    ,  2.55385883033257575402681845385e-13
    , -5.06258237507038698392265499770e-14
    ,  9.89705409478327321641264227110e-15
    , -1.90685978789192181051961024995e-15
    ,  3.50826648032737849245113757340e-16
    ]
erfc_x510_cs =
    [  1.11684990123545698684297865808 / 2
    ,  0.003736240359381998520654927536
    , -0.000916623948045470238763619870
    ,  0.000199094325044940833965078819
    , -0.000040276384918650072591781859
    ,  7.76515264697061049477127605790e-6
    , -1.44464794206689070402099225301e-6
    ,  2.61311930343463958393485241947e-7
    , -4.61833026634844152345304095560e-8
    ,  8.00253111512943601598732144340e-9
    , -1.36291114862793031395712122089e-9
    ,  2.28570483090160869607683087722e-10
    , -3.78022521563251805044056974560e-11
    ,  6.17253683874528285729910462130e-12
    , -9.96019290955316888445830597430e-13
    ,  1.58953143706980770269506726000e-13
    , -2.51045971047162509999527428316e-14
    ,  3.92607828989125810013581287560e-15
    , -6.07970619384160374392535453420e-16
    ,  9.12600607264794717315507477670e-17
    ]
