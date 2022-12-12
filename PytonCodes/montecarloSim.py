# Importing Packages
import matplotlib.pyplot as plt
import random

# BW = input("Birth weight: ")
BW = 50
# growth_rate = random.uniform(0.8, 1.2)

# running simulation
num_simulations = 1000
max_num_days = 365

end_weight = []

#creatig figures
fig = plt.figure()
plt.title("Monte Carlo simulation [" + str(num_simulations) + "simulations]")
plt.xlabel("days")
plt.ylabel("weight")
plt.xlim([0, max_num_days])

# For loop to run for the number of simulations desired
for i in range(num_simulations):
    weights = [BW]
    num_days = [0]
    growth_rate = random.uniform(0.8, 1.2)
   
    while num_days[-1] < max_num_days:
        
        weights.append(weights[-1] + growth_rate)

        num_days.append(num_days[-1] + 1)

    plt.plot(num_days, weights)

# Showing the plot after the simulations are finished
plt.show()

# overall_end_weight = sum(end_weight)/len(end_weight)
# Displaying the averages
# print("Average ending weight " + str(num_simulations) + "runs: $" + str(overall_end_weight))