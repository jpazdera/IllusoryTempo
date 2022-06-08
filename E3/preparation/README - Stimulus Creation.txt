STIMULUS CREATION

The process for creating stimuli follows the steps below:

1) The stimulus creation process begins by running save_tones.rb with Sonic Pi.
This script calls generate_tones.rb, which produces each of the tones to be
used in the study, and then saves them to a WAV file
(../stimuli/tones/raw_tones.wav).

2) Use Audacity to double-check the tuning of each tone. If the pitch of any
tone is noiceably flat/sharp (this seems to be particularly prevalent in the
7th octave) or Audacity detects the pitch as being different from what was
intended, you can use the Change Pitch function to correct the tuning.

3) After correcting any original tones which were out of tune, export the audio
to a new WAV file (../stimuli/tones/raw_tones_corrected.wav).

4) Use the first few cells of Tone Editing ITLT.ipnb to read
raw_tones_corrected.wav, isolate each tone, cut and fade them, and then save
them to individual tone files (../stimuli/tones/piano__.wav).

5) Run tone_balancing.m in MATLAB to load each tone, normalize the perceived 
loudness of each, and save a new version of it 
(../stimuli/tones/piano__-normed.wav).

6) Return to the second half of Tone Editing ITLT.ipynb. The remainder of the
code loads the normed tones and organizes them into the tone sequences used in
the experiment. The resulting sequences are saved in ../stimuli.