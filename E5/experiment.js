/* - - - - LOAD ID - - - - */
var urlvars = jsPsych.data.urlVariables();
var minID = 1;
var maxID = 250;
var subjectID;

 // If participant has a valid ID, use it, otherwise assign a random ID
 if (('participant' in urlvars) && !(isNaN(parseInt(urlvars['participant']))) &&
 (parseInt(urlvars['participant']) >= minID) && (parseInt(urlvars['participant']) <= maxID)) {
     subjectID = parseInt(urlvars['participant']);
 } else {
     subjectID = Math.floor(Math.random() * (maxID - minID + 1)) + minID;
 }

// The register of the tones are randomized for each participant
var octave_range = jsPsych.randomization.sampleWithoutReplacement([0, 1], 1)[0];

jsPsych.data.addProperties({
    subject: subjectID,
    range: octave_range,
    experiment: 'ITSR',
    code_version: 'v1.0'
});

// Use participant ID number to load the trial order, while matching trial order between tap and no-tap participants
$.getJSON(`schedules/session${subjectID}.json`).done(function (schedule) {

    /* - - - - SETTINGS - - - - */

    var n_blocks = 6;
    var trials_per_block = 30;
    var post_instruction_delay = 2000;
    var post_reference_delay = 1800;
    var post_response_delay = 1500;
    var post_audio_delay = 100;

    /* - - - - PAVLOVIA INTEGRATION - - - - */

    var pavlovia_init = {
        type: 'pavlovia',
        command: 'init'
    };

    var pavlovia_finish = {
        type: 'pavlovia',
        command: 'finish'
    };

    /* - - - - INSTRUCTIONS - - - - */

    // Welcome message
    var welcome = {
        type: 'html-keyboard-response',
        data: { event: 'welcome' },
        stimulus: '<p>Welcome to the study! Press any key to begin reading the instructions.</p>'
    };

    // Instructions about requirements for the experiment
    var instructions_requirements = {
        type: 'html-button-response',
        data: { event: 'requirements' },
        choices: ['Continue'],
        stimulus: '<p>Before we begin, please close all other browser tabs and programs that may produce sound ' +
        'alerts or notifications, or which you may find distracting.</p><p>In this study, we will ask you to listen ' +
        'to sequences of sounds and to make some judgments about them. Because of this, it\'s important that you ' +
        'complete this study using headphones while seated in a quiet environment. If it\'s not currently possible ' +
        'for you to satisfy this requirement, please return to complete the experiment at a later time.</p><p>If ' +
        'you are currently using Safari to access the study, please note that the task may not run properly in ' +
        'this browser, and it may prevent you from hearing any sounds. You should copy the URL from this page and ' +
        'paste it into a different web browser to complete the study there, instead. Chrome, Firefox, and Edge are ' +
        'all officially supported. We apologize for any inconvenience!</p><p>If the text instructions appear too ' +
        'large or too small on your screen, you should adjust your browser\'s zoom level at this time. When you are ' +
        'ready to continue, press the button below.</p>'
    };

    // Instructions for pre-experiment headphone test
    var instructions_audio_test = {
        type: 'html-button-response',
        data: { event: 'headphone_test_instructions' },
        choices: ['Start'],
        post_trial_gap: post_instruction_delay,
        stimulus: '<p>First, we\'ll complete an audio test to make sure you can accurately hear the sounds we\'ll be ' +
        'presenting throughout the study. In this test, you will hear several sets of three tones. For each set, ' +
        'your goal is to determine which of the three tones is the <strong>quietest</strong>.</p><p>After hearing ' +
        'each set of tones, you will be prompted to press the 1, 2, or 3 key to indicate whether the first, second, ' +
        'or third tone was the quietest.</p><p>Press the button below to begin the test.</p>'
    };

    // Main task instructions (pre-practice)
    var instructions_main = {
        type: 'html-button-response',
        data: { event: 'main_instructions' },
        choices: ['Start'],
        post_trial_gap: post_instruction_delay,
        stimulus: '<p>Now you are ready to proceed to the main task.</p><p>On each trial, you will hear a series of ' +
            'clicks from a metronome, followed by a repeating tone. Your goal is to determine how much faster or slower ' +
            'this repeating tone is, compared to the metronome.</p><p>After listening to each pair, you will be given a ' +
            'slider with which to respond. The slider will range from "Half as Fast" on the left to "Twice as Fast" on ' +
            'the right. If the repeating tone was slower than the metronome, you should move the slider to the left by ' +
            'a distance which represents how much slower it was. If the repeating tone was faster than the metronome, ' +
            'you should instead move the slider to the right by a distance which represents how much faster it was. If ' +
            'the repeating tone was played at the same rate as the metronome, you should place the slider in the center ' +
            'of the range, where it is marked "Equal Rates".</p><p>Press the button below to try three practice trials, ' +
            'which will help you get comfortable with the task.</p>'
    };

    // Final main task instructions (post-practice)
    var instructions_final = {
        type: 'html-button-response',
        data: { event: 'summary_instructions' },
        choices: ['Start'],
        post_trial_gap: post_instruction_delay,
        stimulus: '<p>You have completed the practice trials and are now ready to begin!</p><p>Remember, your goal ' +
            'is to determine how much faster or slower the repeating tone is compared to the metronome.</p><p>Please ' +
            'note that the trials will be organized into ' + (n_blocks).toString() + ' sections, and you will have the ' +
            'opportunity to rest after each. If the practice trials were too loud or too quiet, adjust your volume ' +
            'now.</p><p><strong>It is critical that you do not adjust your volume any further until the study has ' +
            'concluded.</strong></p><p>Press the button below when you are ready to proceed.</p>'
    };

    // Completion screen
    var ending = {
        type: 'html-button-response',
        data: { event: 'ending' },
        choices: ['Submit'],
        stimulus: '<h3>You have completed section ' + (n_blocks).toString() + ' of ' + (n_blocks).toString() +
            '!</h3><p>Thank you for participating! When you are ready, press the button below to submit your data and ' +
            'complete the study.</p><p>You will receive credit on SONA once we verify that you have completed our ' +
            'study, which will likely occur within the next 24 hours.</p><p>If you have any questions about the study ' +
            'or would like to learn more about our lab\'s work, please contact Jesse Pazdera at pazderaj@mcmaster.ca.' +
            '</p><p><em>Have a great day!</em></p>'
    };

    var debrief = {
        type: 'html-button-response',
        data: { event: 'debrief' },
        choices: ['Exit'],
        stimulus: '<h1>About This Study</h1>' +
            '<h3>Principal Investigator: Dr. Laurel J. Trainor (ljt@mcmaster.ca)</h3>' +
            '<h3>Researcher: Jesse K. Pazdera, B.Sc. (pazderaj@mcmaster.ca)</h3>' +
            '<p style="text-align: justify; text-indent: 35px; max-width: 750px;">In this study, you listened to sequences of metronome ' +
            'clicks and repeating tones and rated the speed of each tone sequence. The metronome always played at the same rate, while the ' +
            'tones varied in both their pitch (how high or low they were) and their tempo (how quickly they repeated). Some tones were played ' +
            'slower than the metronome, some were played faster, and others were played at precisely the same rate. In total, you heard tones ' +
            'played at six different pitches and 15 different rates. We are interested in whether the pitch of a sound sequence can make it ' +
            'appear to be faster or slower than it truly is, as this will help us to better understand how our brains track time. Previous ' +
            'research suggests that people hear higher-pitched speech and music as faster than lower-pitched speech and music, even when both ' +
            'are played at the same rate. However, most experiments have only compared one higher octave to one lower octave, making it ' +
            'difficult to understand how perceived tempo changes across the wider range of sounds we might hear. Recent studies from our lab ' +
            'have addressed this limitation by asking participants to rate tempo across six full octaves (for comparison, most modern pianos ' +
            'span seven). These studies have suggested that middle pitches sound faster than very low <i>and</i> very high pitches.</p>' +
            '<p style="text-align: justify; text-indent: 35px; max-width: 750px;">During the current task, you heard either a collection of ' +
            'low-to-middle tones or a collection of middle-to-high tones, chosen at random. We are interested in whether both groups of ' +
            'people will again hear the tones in middle octaves as fastest. Your responses will help us to understand how and why pitch ' +
            'affects our brains\' processing of time.</p>' +
            '<h3 style="max-width: 750px;">We would like to thank you again for participating in our study. If you would like to learn more ' +
            'about our lab\'s research, please contact Jesse Pazdera at the email listed above. Your responses have been saved and you may ' +
            'close your browser at any time.</h3>' 
    };

    /* - - - - AUDIO TEST - - - - */

    var sound_check_procedure = {
        timeline: [
            // Audio test stimulus
            {
                type: 'audio-keyboard-response',
                data: { event: 'headphone_test_tones' },
                stimulus: jsPsych.timelineVariable('stimulus'),
                response_ends_trial: false,
                trial_ends_after_audio: true,
                prompt: '<h3>Sound check</h3><p>You should hear three tones in a sequence. Try to determine which ' +
                    'tone is the <strong>quietest</strong>.</p>'
            },
            // Audio test response
            {
                type: 'html-keyboard-response',
                data: { event: 'headphone_test_response' },
                choices: ['1', '2', '3', '0'],
                post_trial_gap: post_response_delay,
                stimulus: '<p>Press the 1, 2, or 3 key on your keyboard to indicate which tone was the quietest.</p>' +
                    '<p>If you were not able to hear three tones, check that your headphones are working and that your ' +
                    'volume is turned up, then press 0.</p>'
            }
        ],
        timeline_variables: [
            { stimulus: 'headphone_check/antiphase_HC_IOS.wav' },
            { stimulus: 'headphone_check/antiphase_HC_ISO.wav' },
            { stimulus: 'headphone_check/antiphase_HC_OIS.wav' },
            { stimulus: 'headphone_check/antiphase_HC_OSI.wav' },
            { stimulus: 'headphone_check/antiphase_HC_SIO.wav' },
            { stimulus: 'headphone_check/antiphase_HC_SOI.wav' },
        ],
        randomize_order: true
    };

    /* - - - - PRACTICE TRIALS - - - - */

    // Practice tones use the tone type of the first block
    var timeline_vars
    if (octave_range === 0) {
        timeline_vars = [
            { stimulus: 'stimuli/practice_sequence_0_741.wav', ioi: 741 },
            { stimulus: 'stimuli/practice_sequence_0_550.wav', ioi: 550 },
            { stimulus: 'stimuli/practice_sequence_0_407.wav', ioi: 407 },
        ];
    } else {
        timeline_vars = [
            { stimulus: 'stimuli/practice_sequence_1_741.wav', ioi: 741 },
            { stimulus: 'stimuli/practice_sequence_1_550.wav', ioi: 550 },
            { stimulus: 'stimuli/practice_sequence_1_407.wav', ioi: 407 },
        ];
    }

    var practice_trials = {
        timeline: [
            // Metronome
            {
                type: 'audio-keyboard-response',
                data: { event: 'practice_metronome', pitch: 0, ioi: jsPsych.timelineVariable('ioi'), loudness: 1 },
                stimulus: 'stimuli/reference_sequence.wav',
                response_ends_trial: false,
                trial_ends_after_audio: true,
                post_trial_gap: post_reference_delay
            },
            // Repeating Tone
            {
                type: 'audio-keyboard-response',
                data: { event: 'practice_tones', pitch: 0, ioi: jsPsych.timelineVariable('ioi'), loudness: 1 },
                stimulus: jsPsych.timelineVariable('stimulus'),
                response_ends_trial: false,
                trial_ends_after_audio: true,
                post_trial_gap: post_audio_delay
            },
            // Response Slider
            {
                type: 'html-slider-response',
                data: { event: 'practice_response', pitch: 0, ioi: jsPsych.timelineVariable('ioi'), loudness: 1 },
                stimulus: ['How fast was the repeating tone relative to the metronome?'],
                labels: ['Half as Fast', 'Equal Rates', 'Twice as Fast'],
                require_movement: true,
                post_trial_gap: post_response_delay
            }
        ],
        timeline_variables: timeline_vars,
        randomize_order: false
    };

    /* - - - - BLOCKING - - - - */

    // Start building set of audio files that will need to be pre-loaded
    var audio_files = new Set();
    audio_files.add('stimuli/reference_sequence.wav');
    var headphone_check_strings = ['IOS', 'ISO', 'OIS', 'OSI', 'SIO', 'SOI'];
    var practice_strings = ['741', '550', '407'];
    for (i in headphone_check_strings) {
        s = headphone_check_strings[i];
        audio_files.add(`headphone_check/antiphase_HC_${s}.wav`);
    }
    for (i in practice_strings) {
        s = practice_strings[i];
        audio_files.add(`stimuli/practice_sequence_${octave_range}_${s}.wav`);
    }

    // Add instructions, pre-tests, and practice trials to timeline
    var timeline = [pavlovia_init];
    timeline = timeline.concat([welcome, instructions_requirements]);  // Starting info
    timeline = timeline.concat([instructions_audio_test, sound_check_procedure]);  // Audio test
    timeline = timeline.concat([instructions_main, practice_trials, instructions_final]);  // Task instructions and practice

    // Dynamically construct trials based on schedule, while adding each audio file to the preload list
    for (block = 0; block < n_blocks; block++) {
        for (trial = 0; trial < trials_per_block; trial++) {
            audio_files.add(`stimuli/sequence_${octave_range}_${schedule[block][trial][0]}_${schedule[block][trial][1]}_${schedule[block][trial][2]}.wav`);
            // Metronome
            timeline.push({
                type: 'audio-multiple-response',
                data: { 
                    event: 'metronome',
                    range: `${octave_range}`,
                    pitch: `${schedule[block][trial][0]}`,
                    ioi: `${schedule[block][trial][1]}`,
                    loudness: `${schedule[block][trial][2]}` 
                },
                stimulus: 'stimuli/reference_sequence.wav',
                response_ends_trial: false,
                trial_ends_after_audio: true,
                post_trial_gap: post_reference_delay
            });
            // Repeating Tone
            timeline.push({
                type: 'audio-multiple-response',
                data: { 
                    event: 'tones',
                    range: `${octave_range}`,
                    pitch: `${schedule[block][trial][0]}`,
                    ioi: `${schedule[block][trial][1]}`,
                    loudness: `${schedule[block][trial][2]}` 
                },
                stimulus: `stimuli/sequence_${octave_range}_${schedule[block][trial][0]}_${schedule[block][trial][1]}_${schedule[block][trial][2]}.wav`,
                response_ends_trial: false,
                trial_ends_after_audio: true
            });
            // Response Slider
            timeline.push({
                type: 'html-slider-response',
                data: {
                    event: 'response',
                    range: `${octave_range}`,
                    pitch: `${schedule[block][trial][0]}`,
                    ioi: `${schedule[block][trial][1]}`,
                    loudness: `${schedule[block][trial][2]}` 
                },
                stimulus: ['How fast was the repeating tone relative to the metronome?'],
                labels: ['Half as Fast', 'Equal Rates', 'Twice as Fast'],
                require_movement: true,
                post_trial_gap: post_response_delay
            });
        }
        if (block < n_blocks - 1) {
            // Break period
            timeline.push({
                type: 'html-button-response',
                data: { event: 'break' },
                choices: ['Continue'],
                post_trial_gap: post_instruction_delay,
                stimulus: '<h3>You have completed section ' + (block + 1).toString() + ' of ' + (n_blocks).toString() +
                '!</h3><p>When you are ready to continue, press the button below to begin the next section.</p>' +
                '<p><em>Please remember not to adjust your volume until the study has concluded.</em></p>'
            });
        }
    }

    // Set up experiment conclusion
    timeline.push(ending);
    timeline.push(pavlovia_finish);
    timeline.push(debrief);

    /* - - - - EXECUTION - - - - */

    jsPsych.init({
        timeline: timeline,
        default_iti: 0,
        use_webaudio: true,
        preload_audio: Array.from(audio_files),
        show_preload_progress_bar: true,
        show_progress_bar: true,
        exclusions: {audio: true}
    });
});
