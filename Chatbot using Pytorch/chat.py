import random
import json

import torch

from datetime import datetime
import time
from datetime import timedelta


from model import NeuralNet
from nltk_utils_s import bag_of_words, tokenize

device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

with open('intents.json', 'r') as json_data:
    intents = json.load(json_data)

FILE = "data.pth"
data = torch.load(FILE)

input_size = data["input_size"]
hidden_size = data["hidden_size"]
output_size = data["output_size"]
all_words = data['all_words']
tags = data['tags']
model_state = data["model_state"]

model = NeuralNet(input_size, hidden_size, output_size).to(device)
model.load_state_dict(model_state)
model.eval()


#######Email
import smtplib, ssl
def send_email(message):
            
    def read_creds():
        user = password_1 = ""
        with open("credentials.txt", "r") as f:
            file = f.readlines()
            user = file[0].strip()
            password_1 = file[1].strip()
    
        return user, password_1
    
    
    port = 465
    
    sender, password = read_creds()
    
    recieve = sender
   
    context = ssl.create_default_context()
    
    print("Placing your order...")
    with smtplib.SMTP_SSL("smtp.gmail.com", port, context=context) as server:
        server.login(sender, password)
        server.sendmail(sender, recieve, message)
        
   
    #############


### non veg burger
non_veg = datetime.now()

r_time_n = non_veg + timedelta(minutes=6)
ready_time_n =  r_time_n.strftime("%H:%M:%S")

message_n = """\
Subject: Order for a Non veg Burger

Order for An item of non veg burger!

Order placed\
    
Delivery time: 
""" + str(ready_time_n)

#######pizza and non veg burger
non_pizza = datetime.now()

r_time_p_n = non_pizza + timedelta(minutes=12)
ready_time_p_n =  r_time_p_n.strftime("%H:%M:%S")

message_p_n = """\
Subject: Order for a Pizza and non veg burger

Order for An item of pizza and non veg burger!

Order placed\
    
Delivery time: 
""" + str(ready_time_p_n)


##### pizza veg burger
pizza_veg = datetime.now()

r_time_p_v = pizza_veg + timedelta(minutes=10)
ready_time_p_v =  r_time_p_v.strftime("%H:%M:%S")

message_p_v = """\
Subject: Order for a Pizza and veg Burger

Order for An item of pizza and veg burger!

Order placed\
    
Delivery time: 
""" + str(ready_time_p_v)



##### pizza veg non veg burger
pizza_veg_non = datetime.now()

r_time_p_v_n = pizza_veg_non + timedelta(minutes=15)
ready_time_p_v_n =  r_time_p_v_n.strftime("%H:%M:%S")

message_p_v_n = """\
Subject: Order for a Pizza Veg burger and Non veg Burger

Order for An item of pizza veg burger and Non veg burger!

Order placed\
    
Delivery time: 
""" + str(ready_time_p_v_n)


##### pizza
pizza = datetime.now()

r_time_p = pizza + timedelta(minutes=11) 
ready_time_p = r_time_p.strftime("%H:%M:%S")

message_p = """\
Subject: Order for a Pizza

Order for An item of pizza!

Order placed\
    
Delivery time: 
""" + str(ready_time_p)


#####veg burger
veg = datetime.now()

r_time_v = veg + timedelta(minutes=5) 
ready_time_v = r_time_v.strftime("%H:%M:%S")
message_v = """\
Subject: Order for a veg Burger

Order for An item of veg burger!

Order placed\
    
Delivery time: 
""" + str(ready_time_v)


#####veg and non veg burger
veg_non = datetime.now()
r_time_v_n = veg_non + timedelta(minutes=11) 
ready_time_v_n = r_time_v_n.strftime("%H:%M:%S")

message_v_n = """\
Subject: Order for a veg Burger and non veg burger

Order for An item of veg burger and a non veg burger!

Order placed\
    
Delivery time: 
""" + str(ready_time_v_n)

bot_name = "Fille"
print("Hello! I am Fille!!")
while True:
    # sentence = "do you use credit cards?"
    sentence = input("You: ")
    if sentence == "quit" or sentence == "exit":
        break

    sentence = tokenize(sentence)
    X = bag_of_words(sentence, all_words)
    X = X.reshape(1, X.shape[0])
    X = torch.from_numpy(X).to(device)

    output = model(X)
    _, predicted = torch.max(output, dim=1)

    tag = tags[predicted.item()]

    probs = torch.softmax(output, dim=1)
    prob = probs[0][predicted.item()]
  
    if prob.item() > 0.75:
        for intent in intents['intents']:
            if tag == intent["tag"]:
                print(f"{bot_name}: {random.choice(intent['responses'])}")
            
        if tag == 'pizza':
            send_email(message = message_p)
            print("Order Placed!")
            print("Please proceed to counter to pay your bill and pick your order at: ", ready_time_p)
        
        if tag == 'pizza_veg_burger':
            send_email(message = message_p_v)
            print("Order Placed!")
            print("Please proceed to counter to pay your bill and pick your order at: ", ready_time_p_v)
        
        if tag == 'pizza_veg_non_veg_burger':
            send_email(message = message_p_v_n)
            print("Order Placed!")
            print("Please proceed to counter to pay your bill and pick your order at: ", ready_time_p_v_n)
        
            
        if tag == 'pizza_non_veg_burger':
            send_email(message = message_p_n)
            print("Order Placed!")
            print("Please proceed to counter to pay your bill and pick your order at: ",ready_time_p_n)
        
        if tag == 'non veg burger':
            send_email(message = message_n)
            print("Order Placed!")
            print("Please proceed to counter to pay your bill and pick your order at: ", ready_time_n)
        
        if tag == 'veg_non_veg_burger':
            send_email(message = message_v_n) 
            print("Order Placed!")
            print("Please proceed to counter to pay your bill and pick your order at: ",ready_time_v_n)
        
            
        if tag == 'veg burger': 
            send_email(message = message_v)
            print("Order Placed!") 
            print("Please proceed to counter to pay your bill and pick your order at: ", ready_time_v)
            
    
                    
    else:
        print(f"{bot_name}: I do not understand... Can you please be more clear")