import subprocess
import os

executable = "./interp"
tst_dirs = [
    os.path.join("test", "conc", "single_threaded"),
    os.path.join("test", "conc", "multi_threaded")
    ]

passed = "\033[92mPASSED\033[0m"
failed = "\033[91mFAILED\033[0m"

for tst_dir in tst_dirs:
  print "Running tests in", tst_dir, "..."
  tsts = [f for f in os.listdir(tst_dir)
           if os.path.isfile(os.path.join(tst_dir, f))]
  for tst in sorted(tsts):
    print tst + ":",
    result = subprocess.check_output([executable, os.path.join(tst_dir, tst)])
    should_pass = tst.split("-")[-1] == "PASS"
    if should_pass == (result == ""):
      print passed
    else:
      print failed

