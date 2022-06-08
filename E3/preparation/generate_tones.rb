use_synth :piano
use_synth_defaults vel: 0.7

play :A2
sleep 3

play :A3
sleep 3

play :A4
sleep 3

play :A5
sleep 3

play :A6
sleep 3

# Warning: The A7 on the piano is way flat (try comparing it to an A7 sine wave)
# This will need to be corrected in Audacity afterwards
play :A7
sleep 3

play :Ds5
sleep 3

cue :finish
