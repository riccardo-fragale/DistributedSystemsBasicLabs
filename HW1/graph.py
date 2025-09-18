
import matplotlib.pyplot as plt
import pandas as pd

datap1 = pd.read_csv("Result1.csv", header = None, names=["Iteration","TotalTime"])
datap2 = pd.read_csv("Result2.csv", header = None, names=["Iteration","TotalTime"])


plt.figure(figsize=(8, 5))
plt.plot(datap1["Iteration"], datap1["TotalTime"], marker="o", label="Experiment 1")
plt.plot(datap2["Iteration"], datap2["TotalTime"], marker="s", label="Experiment 2")

plt.xlabel("Iteration")
plt.ylabel("Total Time")
plt.title("Comparison of TotalTime Across Iterations")
plt.legend()
plt.grid(True)

plt.savefig("comparison.png")