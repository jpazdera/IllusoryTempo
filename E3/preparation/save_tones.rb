source_file = "D:/Documents/git/it-long-tone/preparation/generate_tones.rb"

save_file = "D:/Documents/git/it-long-tone/stimuli/tones/raw_tones.wav"

osc_send "localhost", 51235, "/start-recording", "myGUID"

run_file source_file

sync :finish

osc_send "localhost", 51235, "/stop-recording", "myGUID"

sleep 1

osc_send "localhost", 51235, "/save-recording", "myGUID", save_file

sleep 1