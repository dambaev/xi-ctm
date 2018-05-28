# xi-ctm
this is a small tool to detect inversion/rotate/scaling of touch device, based
on the output of `xinput_calibrator` tool.

# The problem, that it trying to solve
We are using touchscreens with intel-nucs boards. Touchscreens needs to be
calibrated. We have used `xinput_calibrator` previously, but it had failed to
detect inversions on new touchscreens. When we had started to investigate the
topic, it turned out, that `xinput_calibrator` uses legacy way to calibrate
the device: it uses min/max values for X and Y axes and this is a problem, because
in order to recalibrate touchscreen, you need to reset those values to defaults,
but you don't know those defaults.
  Instead, current way to calibrate screen is to define Coordinate Transformation
Matrix. One big pros of this is that we know it's default value (which is the
identity matrix).
  At this step, we had decided to not try to patch `xinput_calibrator`, because
it will not be applied to upstream, because the latest commit is 5 years old.
Instead, we decided  to use `xinput_calibrator` as just a frontend to some fake
device. And now it's goal is just to log user's clicks, which will be used by
this tool to provide Coordinate Transformation Matrix.

So, if you had some script like:

```
  xinput_calibrator > config
```

Then now it will be like:

```
  export XICTM_DEVICE="device"
  export DISPLAY=:0
  # reset current calibration values to default
  xinput set-prop $XICTM_DEVICE 'Coordinate Transformation Matrix' 1 0 0 0 1 0 0 0 1
  xinput_calibrator --fake -v --misclick 0 --no-timeout | xi-ctm > config
```

# Building and installing

```
  wget -qO- https://get.haskellstack.org/ | sh
  ./install.sh
  rm -rf ~/.stack
```

# Runtime requirements

* xrandr: used to get display's geometry
* xinput: used to apply calculated transformation matrix

# Usage

xi-ctm is not using command line arguments, instead it uses environment
variables:

* XICTM_DEVICE="device_name" - the name of device to be calibrated
* XICTM_MODE=performance|profile|debug:
  - performance - default mode. Just does it's job
  - profile - outputs the timing information at each step
  - debug - logs debug information to stdout
