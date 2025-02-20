import pyautogui
import time
import math

# install:
#   python3 -m venv serhiienv && source serhiienv/bin/activate
#   pip install --upgrade pip && pip install pillow && pip install python3-xlib && pip install pyautogui
# run: python mouse_cursor.py

def move_mouse_in_circle(radius=100, center_x=960, center_y=540, steps=360, delay=0.01):
    """
    Moves the mouse cursor in a circular path.

    Parameters:
    - radius: Radius of the circle (in pixels).
    - center_x: X-coordinate of the circle's center.
    - center_y: Y-coordinate of the circle's center.
    - steps: Number of points along the circle.
    - delay: Delay in seconds between moves.
    """
    for i in range(steps):
        angle = math.radians(i)
        x = center_x + radius * math.cos(angle)
        y = center_y + radius * math.sin(angle)
        pyautogui.moveTo(x, y)
        time.sleep(delay)

if __name__ == "__main__":
    while True:
        move_mouse_in_circle()