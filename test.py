import subprocess
import os

executable = "./interp"
tst_dirs = [
    os.path.join("test", "conc", "single_threaded"),
    os.path.join("test", "conc", "multi_threaded"),
    os.path.join("test", "sym", "single_threaded"),
    os.path.join("test", "sym", "multi_threaded"),
    os.path.join("test", "interesting"),
    ]

passed = "\033[92mPASSED\033[0m"
failed = "\033[91mFAILED\033[0m"
skipped = "\033[94mSKIPPED\033[0m"

for tst_dir in tst_dirs:
  print "Running tests in", tst_dir, "..."
  tsts = [f for f in os.listdir(tst_dir)
           if os.path.isfile(os.path.join(tst_dir, f))]
  for tst in sorted(tsts):
    print tst + ":",
    key = tst.split("-")[-1]
    if key == "LOOP":
      print skipped 
      continue;
    result = subprocess.check_output("%s %s 2>&1 > /dev/null" %
        (executable, os.path.join(tst_dir, tst)), shell=True)
    if (key == "PASS") == (result == ""):
      print passed
    else:
      print failed

