# Parser und Interpreter für primitiv rekursive Funktionen mit GADTs in Haskell



In diesem Repository befindet sich der Code für mein Masterprojekt. Mit dem vorliegenden Programm werden primitv rekursive Funktionen (siehe Dokumentation: https://github.com/TheGhostOfTomJoad/preks_backend/blob/main/documentation.pdf) geparst, überprüft und ausgewertet.

Das Programm parst zuerst eingegebenen Code und gibt ein Element mit dem folgenden Typ zurück:

```haskell
data ParsedPrek
  = PScucc
  | PConstZero Int
  | PProj Int Int
  | PComp  PrekOrId [PrekOrId]
  | PPrekC PrekOrId PrekOrId
  deriving (Show ,Eq)


data PrekOrId = Pr ParsedPrek | Pid Ident
    deriving (Show ,Eq)
```

Anschließend werden die Variablennamen durch bereits definierte primitiv rekursive Funktionen ersetzt. Fehler können  hier nur autreten, wenn undefinierte Variablen verwendet werden.
Das Resultat hat den Typ:

```haskell
data Prek = ConstZero Int | Succ | Proj Int Int | PrekC Prek Prek | Comp Prek [Prek]
```

In diesem Datentyp können auch invalide Ausdrücke wie   


```haskell
PrekC (ConstZero 0) (ConstZero 0)
```

dargestellt werden. Anschließend wird  versucht diese Ausdrücke zu Ausdrücken vom folgenden Typ zu konvertieren, in dem sich nur valide primitiv rekursive Funktionen darstellen lassen.
```haskell
data CheckedPrek :: Nat -> * where
  CSucc      ::CheckedPrek (Suc Zero)
  CConstZero ::CheckedPrek n
  CProj      ::Fin n -> CheckedPrek n
  CComp      ::CheckedPrek m -> Vec m (CheckedPrek n) 
  -> CheckedPrek n
  CPrekC     ::CheckedPrek n -> CheckedPrek (Suc (Suc n)) -> CheckedPrek (Suc n)
deriving instance Show (CheckedPrek n)
```

Dies ist einerseits nicht immer möglich und andererseits ist die Stelligkeit der geparsten primitiv rekursiven Funktion vorher nicht bekannt.
Deshalb hat die Funktion, mit der die Übersetzung durchgeführt wird, den folgenden Typ:
```haskell

convert :: MonadError [Char] m => Prek -> m ACPrek


data ACPrek :: *  where
    ACPrek ::Natty n -> CheckedPrek n -> ACPrek

data Natty (n :: Nat) where
  Zy ::Natty Zero -- pronounced 'zed-y'
  Sy ::Natty n -> Natty (Suc n) -- pronounced 'ess-y'
```

Die erste Komponente des Konstruktors kann, wie im folgenden Beispiel zu sehen ist, genutz werden um die Stelligkeit von zwei primitiv rekursiven Funktionen zu vergleichen.


```haskell
prekChelper :: MonadError String m => ACPrek -> ACPrek -> m ACPrek
prekChelper (ACPrek n g) (ACPrek (Sy (Sy m)) h) = fmap
    (\proof -> gcastWith proof (ACPrek (Sy m) (CPrekC g h)))
    (testEquality'
        n
        m
        ("The Aritys of the Base Case Function and the Function in the Induction Case should have a difference of two. Thats not true for  "
        ++ show (nattyToInt n)
        ++ " and "
        ++ show (2 + nattyToInt m)
        ++ " "
        )
    )
prekChelper _ _ = throwError
    "The Function for the Inductive Case needs an arity of at least two"
```
Hierbei wird das Paket `Data.Type.Equality` genutzt.

Da Listen auf die selbe Weise übersetzt werden können, ist eine anschließende Auswertung mit der folgenden Funktion möglich:

```haskell
evalSafe :: CheckedPrek n -> Vec n Int -> Int
```

Das Projekt kann hier ausprobiert werden:
https://theghostoftomjoad.github.io/preks_frontend/.


Die Idee, natürliche Zahlen auf Typebene zur Kodierung zu verwenden, stammt aus idesem Post: 
https://softwareengineering.stackexchange.com/questions/276867/is-it-possible-to-bake-dimension-into-a-type-in-haskell.



