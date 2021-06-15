module Main.HCurrying where


class HCurrying a b | a -> b where
  curryH :: a -> b
  uncurryH :: b -> a

instance HCurrying (() -> r) r where
  curryH f = f ()
  uncurryH f ~() = f

instance HCurrying ((a1, ()) -> r) (a1 -> r) where
  curryH f a1 = f (a1, ())
  uncurryH f ~(a1, ()) = f a1

instance HCurrying ((a1, (a2, ())) -> r) (a1 -> a2 -> r) where
  curryH f a1 a2 = f (a1, (a2, ()))
  uncurryH f ~(a1, (a2, ())) = f a1 a2

instance HCurrying ((a1, (a2, (a3, ()))) -> r) (a1 -> a2 -> a3 -> r) where
  curryH f a1 a2 a3 = f (a1, (a2, (a3, ())))
  uncurryH f ~(a1, (a2, (a3, ()))) = f a1 a2 a3

instance HCurrying ((a1, (a2, (a3, (a4, ())))) -> r) (a1 -> a2 -> a3 -> a4 -> r) where
  curryH f a1 a2 a3 a4 = f (a1, (a2, (a3, (a4, ()))))
  uncurryH f ~(a1, (a2, (a3, (a4, ())))) = f a1 a2 a3 a4

instance HCurrying ((a1, (a2, (a3, (a4, (a5, ()))))) -> r) (a1 -> a2 -> a3 -> a4 -> a5 -> r) where
  curryH f a1 a2 a3 a4 a5 = f (a1, (a2, (a3, (a4, (a5, ())))))
  uncurryH f ~(a1, (a2, (a3, (a4, (a5, ()))))) = f a1 a2 a3 a4 a5

instance HCurrying ((a1, (a2, (a3, (a4, (a5, (a6, ())))))) -> r) (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) where
  curryH f a1 a2 a3 a4 a5 a6 = f (a1, (a2, (a3, (a4, (a5, (a6, ()))))))
  uncurryH f ~(a1, (a2, (a3, (a4, (a5, (a6, ())))))) = f a1 a2 a3 a4 a5 a6

instance HCurrying ((a1, (a2, (a3, (a4, (a5, (a6, (a7, ()))))))) -> r) (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r) where
  curryH f a1 a2 a3 a4 a5 a6 a7 = f (a1, (a2, (a3, (a4, (a5, (a6, (a7, ())))))))
  uncurryH f ~(a1, (a2, (a3, (a4, (a5, (a6, (a7, ()))))))) = f a1 a2 a3 a4 a5 a6 a7
