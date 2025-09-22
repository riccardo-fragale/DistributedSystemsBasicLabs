import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

def main():
    # Define directory and file name
    directory = "./test"
    filename = "testLamportQueueLength1.csv"
    filepath = os.path.join(directory, filename)

    # Check if file exists
    if not os.path.exists(filepath):
        print(f"File {filepath} not found.")
        return

    # Load CSV
    df = pd.read_csv(filepath, header=None, names=["Index", "Value"])

    # Ensure Index is numeric
    df["Index"] = pd.to_numeric(df["Index"], errors="coerce")
    df.dropna(inplace=True)

    # Plot graph (line only, white background)
    plt.figure(figsize=(10, 6), facecolor="white")
    plt.plot(df["Index"], df["Value"], linestyle="-", linewidth=1.5, color="blue")

    # Labels & title
    plt.xlabel("number of messages sent")
    plt.ylabel("Holdback queue length")
    plt.title("Holdback Queue Length vs Number of Messages Sent")

    # Set integer ticks for x-axis
    x_min, x_max = int(df["Index"].min()), int(df["Index"].max())
    plt.xticks(np.arange(x_min, x_max + 1, max(1, (x_max - x_min) // 10)))

    plt.grid(True, which="both", axis="y", linestyle="--", linewidth=0.7)

    # Save plot in same directory
    save_path = os.path.join(directory, "PlotTestLamportLength1.png")
    plt.savefig(save_path, bbox_inches="tight")
    print(f"Plot saved as {save_path}")

    # Show plot
    plt.show()

    # Print maximum value
    max_value = df["Value"].max()
    print("Maximum Holdback queue length:", max_value)

if __name__ == "__main__":
    main()

