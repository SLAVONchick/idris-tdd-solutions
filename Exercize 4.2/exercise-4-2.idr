import Data.Vect

data PowerSource = Petrol | Pedal | Electricity

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Tram : (power : Nat) -> Vehicle Electricity
  ElectricCar : (power : Nat) -> Vehicle Electricity


wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4


refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 80
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

vectTake : (n: Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => (pos: Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos [] [] = Nothing
sumEntries {n} pos xs ys = case integerToFin pos n of
  Nothing => Nothing
  Just idx => Just (index idx xs + index idx ys)
