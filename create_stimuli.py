import numpy as np
import matplotlib.pyplot as plt
import os
import csv
import math
from pathlib import Path

# Parameters matching your HTML file
CANVAS_SIZE = 700
DPI = 100
MARGIN = 20
PLOT_SIZE = CANVAS_SIZE - 2 * MARGIN
DOT_RADIUS = 25
X_MIN = -0.1
X_MAX = 1.1
Y_MIN = -0.1
Y_MAX = 1.1

# Dot size in pixels (diameter) at canvas DPI: matplotlib s is area in points², 1 pt = 1/72 inch
DOT_DIAMETER_PX = round(2 * math.sqrt((DOT_RADIUS * 2) / math.pi) * (DPI / 72))

# Fully crossed design: 8 slopes × 8 n values
SLOPES = [-0.595, -0.425, -0.255, -0.085, 0.085, 0.255, 0.425, 0.595]
N_VALUES = [8, 10, 12, 14, 16, 18, 20, 22]
VERSIONS_PER_COMBO = 8

def random_normal(mean=0, std_dev=1):
    """Box-Muller transform for normal distribution"""
    if std_dev == 0:
        return mean
    u1 = np.random.random()
    u2 = np.random.random()
    z0 = np.sqrt(-2 * np.log(u1)) * np.cos(2 * np.pi * u2)
    return z0 * std_dev + mean

def generate_data(n, alpha, sigma):
    """Generate scatterplot data matching your HTML algorithm"""
    valid = False
    x, y = None, None
    
    while not valid:
        # Generate equally spaced x values from 0 to 1
        x = np.linspace(0, 1, n)
        
        # Generate y values: yi = α * xi + εi
        y = np.array([alpha * xi + random_normal(0, sigma) for xi in x])
        
        # Center the dot array: subtract mean so it's centred in the middle
        y = y - np.mean(y)
        
        # Shift to visible range midpoint (0.5)
        y = y + 0.5
        
        # Check if all points are within the visible axis range
        if np.all((y >= Y_MIN) & (y <= Y_MAX)) and np.all((x >= X_MIN) & (x <= X_MAX)):
            valid = True
    
    return x, y

def to_canvas_x(normalized_x):
    """Convert normalized x to canvas x"""
    normalized = (normalized_x - X_MIN) / (X_MAX - X_MIN)
    return MARGIN + normalized * PLOT_SIZE

def to_canvas_y(normalized_y):
    """Convert normalized y to canvas y"""
    normalized = (normalized_y - Y_MIN) / (Y_MAX - Y_MIN)
    return MARGIN + (1 - normalized) * PLOT_SIZE

def generate_and_save_stimulus(stimulus_name, index, output_dir):
    """Generate a single stimulus and save it"""
    # Generate data
    x_norm, y_norm = generate_data(index['n'], index['slope'], index['sigma'])
    
    # Convert to canvas coordinates
    x_canvas = [to_canvas_x(xi) for xi in x_norm]
    y_canvas = [to_canvas_y(yi) for yi in y_norm]
    
    # Create figure matching canvas size
    fig, ax = plt.subplots(figsize=(CANVAS_SIZE/100, CANVAS_SIZE/100), dpi=100)
    ax.set_xlim(0, CANVAS_SIZE)
    ax.set_ylim(0, CANVAS_SIZE)
    ax.invert_yaxis()  # Match canvas coordinate system
    ax.set_aspect('equal')
    
    # Set background color
    fig.patch.set_facecolor('#1a1a1a')
    ax.set_facecolor('#1a1a1a')
    
    # Remove axes and borders
    ax.axis('off')
    
    # Draw scatterplot points
    ax.scatter(x_canvas, y_canvas, s=DOT_RADIUS*2, c='white', edgecolors='none')
    
    # Save file with simple name
    filename = f"{stimulus_name}_{index['index']:04d}.png"
    filepath = output_dir / filename
    plt.savefig(filepath, facecolor='#1a1a1a', bbox_inches='tight', pad_inches=0)
    plt.close()
    
    return filepath, filename, x_norm, y_norm

def main(no_neutral=False, neutral_only=False):
    base_dir = Path('stimuli')
    base_dir.mkdir(exist_ok=True)
    
    sigma = 0.15
    
    if not neutral_only:
        total = len(SLOPES) * len(N_VALUES) * VERSIONS_PER_COMBO  # 8 × 8 × 2 = 128
    
    manifest = []
    global_index = 0
    
    if not neutral_only:
        for slope in SLOPES:
            for n in N_VALUES:
                for version in range(VERSIONS_PER_COMBO):
                    trend_category = 'Trend_Up' if slope > 0 else 'Trend_Down'
                    size_category = 'Big' if n >= 16 else 'Small'
                    
                    filepath, filename, x_values, y_values = generate_and_save_stimulus(
                        f"stim",
                        {
                            'index': global_index,
                            'slope': slope,
                            'n': n,
                            'sigma': sigma
                        },
                        base_dir
                    )
                    
                    manifest.append({
                        'index': global_index,
                        'filename': filename,
                        'path': filename,
                        'slope': slope,
                        'n': n,
                        'sigma': sigma,
                        'version': version,
                        'trend_category': trend_category,
                        'size_category': size_category,
                        'dot_size_px': DOT_DIAMETER_PX,
                        'x_values': ','.join([f'{x:.6f}' for x in x_values]),
                        'y_values': ','.join([f'{y:.6f}' for y in y_values])
                    })
                    
                    global_index += 1
                    if global_index % 20 == 0:
                        print(f"Progress: {global_index}/{total} stimuli generated...")
    
    if not neutral_only:
        manifest_path = base_dir / 'manifest.csv'
        with open(manifest_path, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=manifest[0].keys())
            writer.writeheader()
            writer.writerows(manifest)
        
        print(f"\nDone! Generated {global_index} stimuli total")
        print(f"  8 slopes × 8 n values × {VERSIONS_PER_COMBO} versions = {total}")
        print(f"  Manifest saved to {manifest_path}")
    
    if no_neutral and not neutral_only:
        return
    
    # Generate 20 separate neutral stimuli with their own manifest
    neutral_dir = base_dir / 'neutral'
    neutral_dir.mkdir(exist_ok=True)
    neutral_manifest = []
    
    for i in range(20):
        x_norm, y_norm = generate_data(15, 0, sigma)
        
        # Calculate observed regression slope from the generated points
        x_arr = np.array(x_norm)
        y_arr = np.array(y_norm)
        n = len(x_arr)
        observed_slope = (n * np.sum(x_arr * y_arr) - np.sum(x_arr) * np.sum(y_arr)) / \
                         (n * np.sum(x_arr**2) - np.sum(x_arr)**2)
        
        x_canvas = [to_canvas_x(xi) for xi in x_norm]
        y_canvas = [to_canvas_y(yi) for yi in y_norm]
        
        fig, ax = plt.subplots(figsize=(CANVAS_SIZE/100, CANVAS_SIZE/100), dpi=100)
        ax.set_xlim(0, CANVAS_SIZE)
        ax.set_ylim(0, CANVAS_SIZE)
        ax.invert_yaxis()
        ax.set_aspect('equal')
        fig.patch.set_facecolor('#1a1a1a')
        ax.set_facecolor('#1a1a1a')
        ax.axis('off')
        ax.scatter(x_canvas, y_canvas, s=DOT_RADIUS*2, c='white', edgecolors='none')
        
        filename = f"neutral_{i:04d}.png"
        plt.savefig(neutral_dir / filename, facecolor='#1a1a1a', bbox_inches='tight', pad_inches=0)
        plt.close()
        
        neutral_manifest.append({
            'index': i,
            'filename': filename,
            'path': f"neutral/{filename}",
            'slope_param': 0,
            'observed_slope': round(observed_slope, 6),
            'n': 15,
            'sigma': sigma,
            'dot_size_px': DOT_DIAMETER_PX,
            'x_values': ','.join([f'{x:.6f}' for x in x_norm]),
            'y_values': ','.join([f'{y:.6f}' for y in y_norm])
        })
    
    neutral_manifest_path = base_dir / 'manifest_neutral.csv'
    with open(neutral_manifest_path, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=neutral_manifest[0].keys())
        writer.writeheader()
        writer.writerows(neutral_manifest)
    
    print(f"\nGenerated 20 neutral stimuli in {neutral_dir}/")
    print(f"  Neutral manifest saved to {neutral_manifest_path}")

if __name__ == '__main__':
    import sys
    neutral_only = '--neutral-only' in sys.argv
    no_neutral = '--no-neutral' in sys.argv
    main(no_neutral=no_neutral, neutral_only=neutral_only)
