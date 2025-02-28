class User:
    def __init__(self, name: str, phone: str, email: str):
        self.name = name
        self.phone = phone
        self.email = email
        self.account = None
        self.tracker = []

class Account:
    def __init__(self, username: str):
        self.username = username

class Tracker:
    def __init__(self, tracker_id: int):
        self.tracker_id = tracker_id

class Rider(User):
    def __init__(self, name: str, phone: str, email: str, rider_id: int, rating: int):
        super().__init__(name, phone, email)
        self.rider_id = rider_id
        self.rating = rating

class Driver(User):
    def __init__(self, name: str, phone: str, email: str, driver_id: int, rating: int):
        super().__init__(name, phone, email)
        self.driver_id = driver_id
        self.rating = rating
        self.vehicles = []

class Vehicle:
    def __init__(self, make: str, model: str, year: int, vehicle_type: str):
        self.make = make
        self.model = model
        self.year = year
        self.vehicle_type = vehicle_type

class Order:
    def __init__(self, date: str, time: str):
        self.date = date
        self.time = time
        self.ride = None

class Ride:
    def __init__(self, location: str, duration: int):
        self.location = location
        self.duration = duration
        self.price = None

class Price:
    def __init__(self, subtotal: int, discount: int, total: int):
        self.subtotal = subtotal
        self.discount = discount
        self.total = total

class PaymentMethod:
    def __init__(self, card_type: str, card_number: int, expiration_date: str, ccv: int):
        self.card_type = card_type
        self.card_number = card_number
        self.expiration_date = expiration_date
        self.ccv = ccv
