data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Unicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Tram : (electricity : Nat) -> Vehicle Electric
  ElectricCar : (electricity : Nat) -> Vehicle Electric


wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels (Tram electricity) = 6
wheels (ElectricCar electricity) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 25
refuel Bicycle impossible
refuel Unicycle impossible
refuel (Tram electricity) impossible
refuel (ElectricCar electricity) impossible

recharge : Vehicle Electric -> Vehicle Electric
recharge (Tram electricity) = Tram 200
recharge (ElectricCar electricity) = ElectricCar 200
