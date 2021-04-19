let fuel_for_mass mass = (mass / 3) - 2

let fuel_required input = List.map fuel_for_mass input
