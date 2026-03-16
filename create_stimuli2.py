#!/usr/bin/env python3
"""
Stimulus generator v2: Regenerates until observed slope and residual variance
fall within specified ranges for each of the 8 slope levels.
Output goes to stimuli2/ folder.
"""

import csv
import math
import shutil
from pathlib import Path

import numpy as np
import matplotlib.pyplot as plt

# Parameters matching your HTML file
CANVAS_SIZE = 900
DPI = 100
MARGIN = 20
PLOT_SIZE = CANVAS_SIZE - 2 * MARGIN
DOT_RADIUS = 25
X_MIN = -0.27
X_MAX = 1.27
Y_MIN = -0.27
Y_MAX = 1.27

DOT_DIAMETER_PX = round(2 * math.sqrt((DOT_RADIUS * 2) / math.pi) * (DPI / 72))

# 8 slope levels: 4 positive (from table) mirrored to negative
SLOPES = [-0.3153, -0.2217, -0.1317, -0.0437, 0.0437, 0.1317, 0.2217, 0.3153]
N_VALUES = [8, 10, 12, 14, 16, 18, 20, 22]
VERSIONS_PER_COMBO = 8  # 8 × 8 × 8 = 512 main stimuli
TRAINING_VERSIONS = 1    # 8 × 8 × 1 = 64 training stimuli

# For each of 8 slope levels (index 0-7): (min_observed_slope, max_observed_slope)
# From table: Slopes 0.0437, 0.1317, 0.2217, 0.3153 | Min/Max per level
SLOPE_RANGES = [
    (-0.3346, -0.2962),   # level 0: target -0.3153
    (-0.2401, -0.2035),   # level 1: target -0.2217
    (-0.1495, -0.1139),   # level 2: target -0.1317
    (-0.0612, -0.0262),   # level 3: target -0.0437
    (0.0262, 0.0612),     # level 4: target +0.0437
    (0.1139, 0.1495),     # level 5: target +0.1317
    (0.2035, 0.2401),     # level 6: target +0.2217
    (0.2962, 0.3346),     # level 7: target +0.3153
]

# Single range for residual variance (same for all levels).
# With sigma=0.15, theoretical Var(residuals) ≈ σ² = 0.0225. Allow sampling variation.
RESIDUAL_VAR_MIN, RESIDUAL_VAR_MAX = 0.01, 0.035

# Set to True for verbose attempt/debug info
DEBUG = True


def random_normal(mean=0, std_dev=1):
    """Box-Muller transform for normal distribution"""
    if std_dev == 0:
        return mean
    u1 = np.random.random()
    u2 = np.random.random()
    z0 = np.sqrt(-2 * np.log(u1)) * np.cos(2 * np.pi * u2)
    return z0 * std_dev + mean


def generate_data(n, alpha, sigma):
    """Generate scatterplot data"""
    valid = False
    x, y = None, None
    out_of_bounds_retries = 0

    while not valid:
        x = np.linspace(0, 1, n)
        y = np.array([alpha * xi + random_normal(0, sigma) for xi in x])
        y = y - np.mean(y)
        y = y + 0.5

        if np.all((y >= Y_MIN) & (y <= Y_MAX)) and np.all((x >= X_MIN) & (x <= X_MAX)):
            valid = True
        else:
            out_of_bounds_retries += 1

    return x, y, out_of_bounds_retries


def to_canvas_x(normalized_x):
    normalized = (normalized_x - X_MIN) / (X_MAX - X_MIN)
    return MARGIN + normalized * PLOT_SIZE


def to_canvas_y(normalized_y):
    normalized = (normalized_y - Y_MIN) / (Y_MAX - Y_MIN)
    return MARGIN + (1 - normalized) * PLOT_SIZE


def _observed_slope(x_values, y_values, fallback=0):
    x_arr = np.array(x_values)
    y_arr = np.array(y_values)
    nn = len(x_arr)
    if nn <= 1:
        return fallback
    num = nn * np.sum(x_arr * y_arr) - np.sum(x_arr) * np.sum(y_arr)
    den = nn * np.sum(x_arr**2) - np.sum(x_arr)**2
    return num / den if den != 0 else fallback


def _residual_variance(x_values, y_values):
    """Residual variance from OLS regression: Var(y - ŷ)"""
    x_arr = np.array(x_values)
    y_arr = np.array(y_values)
    nn = len(x_arr)
    if nn <= 2:
        return 0.0
    slope = _observed_slope(x_values, y_values, fallback=0)
    intercept = np.mean(y_arr) - slope * np.mean(x_arr)
    y_pred = slope * x_arr + intercept
    residuals = y_arr - y_pred
    return np.var(residuals)


def _slope_level_index(slope):
    """Return index 0-7 for the given slope"""
    for i, s in enumerate(SLOPES):
        if abs(slope - s) < 1e-9:
            return i
    return 0  # fallback


def generate_and_save_stimulus_v2(stimulus_name, index, output_dir):
    """Generate stimulus, regenerating until slope and residual var are in range"""
    slope = index['slope']
    slope_level = _slope_level_index(slope)
    slope_lo, slope_hi = min(SLOPE_RANGES[slope_level]), max(SLOPE_RANGES[slope_level])

    total_out_of_bounds = 0
    sign_mismatch_retries = 0
    range_reject_retries = 0

    while True:
        x_norm, y_norm, out_of_bounds_retries = generate_data(
            index['n'], slope, index['sigma']
        )
        total_out_of_bounds += out_of_bounds_retries

        # Sign must match for non-zero slope
        if slope != 0:
            obs_slope = _observed_slope(x_norm, y_norm, fallback=slope)
            if np.sign(obs_slope) != np.sign(slope):
                sign_mismatch_retries += 1
                continue

        obs_slope = _observed_slope(x_norm, y_norm, fallback=slope)
        res_var = _residual_variance(x_norm, y_norm)

        # Check slope in range
        if obs_slope < slope_lo or obs_slope > slope_hi:
            range_reject_retries += 1
            if DEBUG and (range_reject_retries + sign_mismatch_retries) in (1, 5, 25, 50, 100, 250):
                print(f"  [attempt {range_reject_retries + sign_mismatch_retries + 1}] slope {obs_slope:.4f} outside [{slope_lo:.3f}, {slope_hi:.3f}]", flush=True)
            continue

        # Check residual variance in range (σ²≈0.0225 for sigma=0.15)
        if res_var < RESIDUAL_VAR_MIN or res_var > RESIDUAL_VAR_MAX:
            range_reject_retries += 1
            if DEBUG and (range_reject_retries + sign_mismatch_retries) in (1, 5, 25, 50, 100, 250):
                print(f"  [attempt {range_reject_retries + sign_mismatch_retries + 1}] res_var {res_var:.5f} outside [{RESIDUAL_VAR_MIN}, {RESIDUAL_VAR_MAX}]", flush=True)
            continue

        break

    # Save image
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

    filename = f"{stimulus_name}_{index['index']:04d}.png"
    filepath = output_dir / filename
    plt.savefig(filepath, facecolor='#1a1a1a', bbox_inches='tight', pad_inches=0)
    plt.close()

    return (
        filepath, filename, x_norm, y_norm,
        total_out_of_bounds, sign_mismatch_retries, range_reject_retries,
        obs_slope, res_var
    )


def main(no_neutral=False, neutral_only=False, training_only=False):
    base_dir = Path(__file__).parent / 'stimuli2'
    base_dir.mkdir(exist_ok=True)
    sigma = 0.15

    if training_only:
        training_dir = base_dir / 'training'
        training_dir.mkdir(exist_ok=True)
        training_manifest = []
        train_index = 0
        total_train = len(SLOPES) * len(N_VALUES) * TRAINING_VERSIONS
        print(f"Generating {total_train} training stimuli...", flush=True)
        if DEBUG:
            print("  [DEBUG] will print attempt counts for stimuli that needed retries", flush=True)

        for slope in SLOPES:
            for n in N_VALUES:
                for version in range(TRAINING_VERSIONS):
                    trend_category = 'Trend_Up' if slope > 0 else 'Trend_Down'
                    size_category = 'Big' if n >= 16 else 'Small'

                    result = generate_and_save_stimulus_v2(
                        "train",
                        {'index': train_index, 'slope': slope, 'n': n, 'sigma': sigma},
                        training_dir
                    )
                    (filepath, filename, x_values, y_values,
                     ob_retries, sign_retries, range_retries,
                     obs_slope, res_var) = result

                    training_manifest.append({
                        'index': train_index,
                        'filename': filename,
                        'path': f'training/{filename}',
                        'slope': slope,
                        'observed_slope': round(obs_slope, 6),
                        'residual_variance': round(res_var, 6),
                        'n': n,
                        'sigma': sigma,
                        'version': version,
                        'trend_category': trend_category,
                        'size_category': size_category,
                        'dot_size_px': DOT_DIAMETER_PX,
                        'out_of_bounds_retries': ob_retries,
                        'sign_mismatch_retries': sign_retries,
                        'range_reject_retries': range_retries,
                        'stimulus_type': 'training',
                        'x_values': ','.join([f'{x:.6f}' for x in x_values]),
                        'y_values': ','.join([f'{y:.6f}' for y in y_values])
                    })

                    total_attempts = 1 + ob_retries + sign_retries + range_retries
                    if DEBUG and total_attempts > 1:
                        print(f"  stim {train_index}: {total_attempts} attempts (ob:{ob_retries} sign:{sign_retries} range:{range_retries})", flush=True)
                    train_index += 1
                    if train_index % 20 == 0:
                        print(f"Progress: {train_index}/{total_train} training stimuli...", flush=True)

        manifest_path = base_dir / 'manifest_training.csv'
        with open(manifest_path, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=training_manifest[0].keys())
            writer.writeheader()
            writer.writerows(training_manifest)

        if DEBUG and training_manifest:
            tot_ob = sum(m['out_of_bounds_retries'] for m in training_manifest)
            tot_sign = sum(m['sign_mismatch_retries'] for m in training_manifest)
            tot_range = sum(m['range_reject_retries'] for m in training_manifest)
            print(f"\n  [DEBUG] Training total retries: ob:{tot_ob} sign:{tot_sign} range:{tot_range}", flush=True)
        print(f"\nDone! {train_index} training stimuli in {base_dir}/")
        return

    if not neutral_only:
        total = len(SLOPES) * len(N_VALUES) * VERSIONS_PER_COMBO
        manifest = []
        global_index = 0
        print(f"Generating {total} stimuli (may take several minutes)...", flush=True)
        if DEBUG:
            print("  [DEBUG] will print attempt counts for stimuli that needed retries", flush=True)

        for slope in SLOPES:
            for n in N_VALUES:
                for version in range(VERSIONS_PER_COMBO):
                    trend_category = 'Trend_Up' if slope > 0 else 'Trend_Down'
                    size_category = 'Big' if n >= 16 else 'Small'

                    result = generate_and_save_stimulus_v2(
                        "stim",
                        {'index': global_index, 'slope': slope, 'n': n, 'sigma': sigma},
                        base_dir
                    )
                    (filepath, filename, x_values, y_values,
                     ob_retries, sign_retries, range_retries,
                     obs_slope, res_var) = result

                    manifest.append({
                        'index': global_index,
                        'filename': filename,
                        'path': filename,
                        'slope': slope,
                        'observed_slope': round(obs_slope, 6),
                        'residual_variance': round(res_var, 6),
                        'n': n,
                        'sigma': sigma,
                        'version': version,
                        'trend_category': trend_category,
                        'size_category': size_category,
                        'dot_size_px': DOT_DIAMETER_PX,
                        'out_of_bounds_retries': ob_retries,
                        'sign_mismatch_retries': sign_retries,
                        'range_reject_retries': range_retries,
                        'x_values': ','.join([f'{x:.6f}' for x in x_values]),
                        'y_values': ','.join([f'{y:.6f}' for y in y_values])
                    })

                    total_attempts = 1 + ob_retries + sign_retries + range_retries
                    if DEBUG and total_attempts > 1:
                        print(f"  stim {global_index}: {total_attempts} attempts (ob:{ob_retries} sign:{sign_retries} range:{range_retries})", flush=True)
                    global_index += 1
                    if global_index % 20 == 0:
                        print(f"Progress: {global_index}/{total} stimuli...", flush=True)

        manifest_path = base_dir / 'manifest.csv'
        with open(manifest_path, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=manifest[0].keys())
            writer.writeheader()
            writer.writerows(manifest)

        if DEBUG and manifest:
            tot_ob = sum(m['out_of_bounds_retries'] for m in manifest)
            tot_sign = sum(m['sign_mismatch_retries'] for m in manifest)
            tot_range = sum(m['range_reject_retries'] for m in manifest)
            print(f"\n  [DEBUG] Total retries: ob:{tot_ob} sign:{tot_sign} range:{tot_range}", flush=True)
        print(f"\nDone! {global_index} stimuli in {base_dir}/")
        print(f"  Manifest: {manifest_path}")

        # Generate training stimuli (same structure as --training but inline)
        training_dir = base_dir / 'training'
        training_dir.mkdir(exist_ok=True)
        training_manifest = []
        train_index = 0
        total_train = len(SLOPES) * len(N_VALUES) * TRAINING_VERSIONS
        print(f"\nGenerating {total_train} training stimuli...", flush=True)
        if DEBUG:
            print("  [DEBUG] will print attempt counts for stimuli that needed retries", flush=True)
        for slope in SLOPES:
            for n in N_VALUES:
                for version in range(TRAINING_VERSIONS):
                    trend_category = 'Trend_Up' if slope > 0 else 'Trend_Down'
                    size_category = 'Big' if n >= 16 else 'Small'
                    result = generate_and_save_stimulus_v2(
                        "train", {'index': train_index, 'slope': slope, 'n': n, 'sigma': sigma}, training_dir
                    )
                    (filepath, filename, x_values, y_values,
                     ob_retries, sign_retries, range_retries, obs_slope, res_var) = result
                    training_manifest.append({
                        'index': train_index, 'filename': filename, 'path': f'training/{filename}',
                        'slope': slope, 'observed_slope': round(obs_slope, 6), 'residual_variance': round(res_var, 6),
                        'n': n, 'sigma': sigma, 'version': version, 'trend_category': trend_category,
                        'size_category': size_category, 'dot_size_px': DOT_DIAMETER_PX,
                        'out_of_bounds_retries': ob_retries, 'sign_mismatch_retries': sign_retries,
                        'range_reject_retries': range_retries, 'stimulus_type': 'training',
                        'x_values': ','.join([f'{x:.6f}' for x in x_values]),
                        'y_values': ','.join([f'{y:.6f}' for y in y_values])
                    })
                    total_attempts = 1 + ob_retries + sign_retries + range_retries
                    if DEBUG and total_attempts > 1:
                        print(f"  stim {train_index}: {total_attempts} attempts (ob:{ob_retries} sign:{sign_retries} range:{range_retries})", flush=True)
                    train_index += 1
                    if train_index % 20 == 0:
                        print(f"Progress: {train_index}/{total_train} training stimuli...", flush=True)
        with open(base_dir / 'manifest_training.csv', 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=training_manifest[0].keys())
            writer.writeheader()
            writer.writerows(training_manifest)
        print(f"  {train_index} training stimuli → {base_dir}/manifest_training.csv")

    if no_neutral and not neutral_only:
        return

    # Neutral stimuli: one neutral.png at root, plus 20 in neutral/ for manifest
    neutral_dir = base_dir / 'neutral'
    neutral_dir.mkdir(exist_ok=True)
    neutral_manifest = []

    for i in range(20):
        x_norm, y_norm, ob_retries = generate_data(15, 0, sigma)
        obs_slope = _observed_slope(x_norm, y_norm, fallback=0)
        res_var = _residual_variance(x_norm, y_norm)

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
        neutral_path = neutral_dir / filename
        plt.savefig(neutral_path, facecolor='#1a1a1a', bbox_inches='tight', pad_inches=0)
        plt.close()
        if i == 0:
            shutil.copy(neutral_path, base_dir / 'neutral.png')
            print(f"  neutral.png saved at {base_dir}/neutral.png")

        neutral_manifest.append({
            'index': i,
            'filename': filename,
            'path': f"neutral/{filename}",
            'slope_param': 0,
            'observed_slope': round(obs_slope, 6),
            'residual_variance': round(res_var, 6),
            'n': 15,
            'sigma': sigma,
            'dot_size_px': DOT_DIAMETER_PX,
            'out_of_bounds_retries': ob_retries,
            'x_values': ','.join([f'{x:.6f}' for x in x_norm]),
            'y_values': ','.join([f'{y:.6f}' for y in y_norm])
        })

    with open(base_dir / 'manifest_neutral.csv', 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=neutral_manifest[0].keys())
        writer.writeheader()
        writer.writerows(neutral_manifest)
    print(f"  20 neutral stimuli in {neutral_dir}/")


if __name__ == '__main__':
    import sys
    neutral_only = '--neutral-only' in sys.argv
    no_neutral = '--no-neutral' in sys.argv
    training_only = '--training' in sys.argv
    main(no_neutral=no_neutral, neutral_only=neutral_only, training_only=training_only)
