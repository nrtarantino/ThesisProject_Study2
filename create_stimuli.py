import numpy as np
import matplotlib.pyplot as plt
import os
import csv
from pathlib import Path

# Parameters matching your HTML file
CANVAS_SIZE = 700  # Reduced from 900
MARGIN = 70  # Proportionally reduced
PLOT_SIZE = CANVAS_SIZE - 2 * MARGIN
DOT_RADIUS = 4  # Slightly smaller dots
X_MIN = -0.3
X_MAX = 1.3
Y_MIN = -0.3
Y_MAX = 1.3

# Stimulus definitions (matching your HTML)
STIMULUS_TREND_DOWN = {
    'name': 'Trend_Down',
    'pairs': [
        {'slope': -0.375, 'n': 15 },
        {'slope': -0.3, 'n': 15 },
        {'slope': -0.225, 'n': 15},
        {'slope': -0.075, 'n': 15 }
    ]
}

STIMULUS_TREND_UP = {
    'name': 'Trend_Up',
    'pairs': [
        {'slope': 0.375, 'n': 15},
        {'slope': 0.3, 'n': 15},
        {'slope': 0.225, 'n': 15},
        {'slope': 0.075, 'n': 15}
    ]
}

STIMULUS_SMALL = {
    'name': 'Small',
    'pairs': [
        {'slope': 0, 'n': 8 },
        {'slope': 0, 'n': 10 },
        {'slope': 0, 'n': 12 },
        {'slope': 0, 'n': 14 }
    ]
}

STIMULUS_BIG = {
    'name': 'Big',
    'pairs': [
        {'slope': 0, 'n': 22},
        {'slope': 0, 'n': 20},
        {'slope': 0, 'n': 18},
        {'slope': 0, 'n': 16}
    ]
}

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
        
        # Adjust vertically so center of mass (mean) of y-values is at 0.5
        mean_y = np.mean(y)
        adjustment = 0.5 - mean_y
        y = y + adjustment
        
        # Check if all points are within bounds AFTER adjustment
        if np.all((y >= -0.27) & (y <= 1.27)):
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

def main():
    # Create output directories
    base_dir = Path('stimuli')
    base_dir.mkdir(exist_ok=True)
    
    # Noise levels to generate (matching your blocks)
    noise_levels = [0.2]  # Add more if you have multiple blocks with different noise
    
    # Generate exactly 32 versions of each slope/n combination
    # Total: 4 stimulus types × 4 pairs × 32 versions = 512 stimuli
    num_versions_per_pair = 32
    
    stimuli_config = [
        (STIMULUS_TREND_UP['name'], STIMULUS_TREND_UP),
        (STIMULUS_TREND_DOWN['name'], STIMULUS_TREND_DOWN),
        (STIMULUS_SMALL['name'], STIMULUS_SMALL),
        (STIMULUS_BIG['name'], STIMULUS_BIG)
    ]
    
    manifest = []
    total_generated = 0
    global_index = 0
    
    for stimulus_name, stimulus_def in stimuli_config:
        stim_dir = base_dir / stimulus_name.lower()
        stim_dir.mkdir(exist_ok=True)
        
        for pair_idx, pair in enumerate(stimulus_def['pairs']):
            slope = pair['slope']
            n = pair['n']
            
            for sigma in noise_levels:
                for version in range(num_versions_per_pair):
                    # Generate and save stimulus
                    filepath, filename, x_values, y_values = generate_and_save_stimulus(
                        stimulus_name,
                        {
                            'index': global_index,
                            'slope': slope,
                            'n': n,
                            'sigma': sigma
                        },
                        stim_dir
                    )
                    
                    # Create manifest entry
                    relative_path = f"{stimulus_name.lower()}/{filename}"
                    manifest_entry = {
                        'index': global_index,
                        'filename': filename,
                        'path': relative_path,
                        'stimulus_name': stimulus_name,
                        'pair_idx': pair_idx,
                        'slope': slope,
                        'n': n,
                        'sigma': sigma,
                        'version': version,
                        'x_values': ','.join([f'{x:.6f}' for x in x_values]),
                        'y_values': ','.join([f'{y:.6f}' for y in y_values])
                    }
                    
                    manifest.append(manifest_entry)
                    total_generated += 1
                    global_index += 1
                    
                    if total_generated % 50 == 0:
                        print(f"Progress: {total_generated}/512 stimuli generated...")
    
    # Save manifest as CSV
    manifest_path = base_dir / 'manifest.csv'
    if manifest:
        with open(manifest_path, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=manifest[0].keys())
            writer.writeheader()
            writer.writerows(manifest)
    
    print(f"\nDone! Generated {total_generated} stimuli total")
    print(f"Breakdown:")
    print(f"  - {STIMULUS_TREND_UP['name']}: 128 images (32 × 4 pairs)")
    print(f"  - {STIMULUS_TREND_DOWN['name']}: 128 images (32 × 4 pairs)")
    print(f"  - Small: 128 images (32 × 4 pairs)")
    print(f"  - Big: 128 images (32 × 4 pairs)")
    print(f"  - Total: {total_generated} stimuli saved in {base_dir}/")
    print(f"  - Manifest saved to {manifest_path}")

if __name__ == '__main__':
    main()
