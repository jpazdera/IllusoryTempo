% Set target loudness
sone_target = 48;  % Was 50 for IT1, lowered to 48 for ITLT to prevent A2 from clipping

% Set required precision of the loudness (in sones)
sone_threshold = .01;

% Set the interval by which the amplitude will be adjusted on each iteration
scaling_interval = .0001;

% Iteratively adjust the loudness of each tone until it matches the metronome
names = ["pianoA2" "pianoA3" "pianoA4" "pianoA5" "pianoA6" "pianoA7" "pianoD#5"];
for name = names
    
    [audioIn, fs] = audioread("../stimuli/tones/" + name + ".wav");
    loudness = acoustic_loudness(audioIn, fs);

    scale = 1;
    while (loudness > sone_target + sone_threshold) || (loudness < sone_target - sone_threshold)
        if loudness > sone_target
            scale = scale - scaling_interval;
        else
            scale = scale + scaling_interval;
        end
        loudness = acoustic_loudness(audioIn * scale, fs);
    end
    
    disp([name, scale, loudness, max(abs(audioIn * scale))])
    
    audiowrite("../stimuli/tones/" + name + "-normed.wav", audioIn * scale, fs);
end
