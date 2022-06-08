/* - - - - LOAD ID - - - - */
var urlvars = jsPsych.data.urlVariables();
var minID = 1;
var maxID = 300;
var subjectID;
if (('participant' in urlvars) && !(isNaN(parseInt(urlvars['participant']))) && (parseInt(urlvars['participant']) >= minID) && (parseInt(urlvars['participant']) <= maxID)) {
    subjectID = parseInt(urlvars['participant']);
} else {  // Randomize ID if participant has invalid or missing ID
    subjectID = Math.floor(Math.random() * (maxID - minID + 1)) + minID;
}

var tap_condition = jsPsych.randomization.sampleWithoutReplacement([false, true], 1)[0];
jsPsych.data.addProperties({
    subject: subjectID,
    tap_condition: tap_condition,
    experiment: 'ITLT',
    code_version: 'v1.0'
});


// Use participant ID number to load the trial order, while matching trial order between tap and no-tap participants
$.getJSON(`schedules/session${subjectID}.json`).done(function (schedule) {

    /* - - - - SETTINGS - - - - */

    var trials_per_block = 30;
    var tap_test_duration = 15000;
    var post_instruction_delay = 2000;
    var post_reference_delay = 1800;
    var post_response_delay = 1500;

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
        stimulus: '<p>Before we begin, please close all other browser tabs and programs that may produce sound alerts or notifications, or which may be distracting.</p>' +
            '<p>In this study, we will ask you to listen to sequences of sounds and to make judgments about them. Therefore, we require ' +
            'that you complete this task using headphones, while seated in a quiet environment. If it is not currently possible for you to satisfy these ' +
            'requirements, please return at a later time to complete the experiment.</p><p>If you are currently accessing the experiment in either Safari or ' +
            'Internet Explorer, please note that the task will NOT run correctly in these browsers. You should copy the URL from this page and paste it into a ' +
            'different web browser to complete the study there, instead. Chrome, Firefox, and Edge are all officially supported.</p><p>If the text ' +
            'instructions appear too large or too small on your screen, you should adjust your browser\'s zoom level at this time. When you are ready to continue, press ' +
            'the button below.</p>'
    };

    // Instructions for pre-experiment headphone test
    var instructions_audio_test = {
        type: 'html-button-response',
        data: { event: 'headphone_test_instructions' },
        choices: ['Start'],
        post_trial_gap: post_instruction_delay,
        stimulus: '<p>Before you proceed to the main task, you will be asked to perform an audio test that will require headphones in order for you to respond correctly.</p>' +
            '<p>In this test, you will hear several sets of three tones. For each set, your goal is to determine which of the three tones is the quietest.</p><p>After ' +
            'hearing each set of tones, you will be prompted to press the 1, 2, or 3 key to indicate whether the first, second, or third tone was the quietest.</p>' +
            '<p>Press the button below to begin the test.</p>'
    };

    // Instructions for pre-experiment tapping task
    var instructions_tapping_test = {
        type: 'html-keyboard-response',
        data: { event: 'tap_task_instructions' },
        choices: [32],
        post_trial_gap: post_instruction_delay,
        stimulus: '<p>Next, you will be asked to tap your finger for a short time at the rate that feels most natural and comfortable to you.</p><p>If you are right-handed, ' +
            'you should tap the J key with your right index finger during this task. If you are left-handed, you should instead tap the F key with your left index finger.</p>' +
            '<p>While completing this exercise, you should rest your wrist on a solid surface and keep your hand still, moving only your index finger to tap the specified ' +
            'key. Go ahead and place your hand in the appropriate position at this time. You should begin tapping when you see a cross (+) appear on the screen, and you should ' +
            'continue tapping until it disappears.</p><p>Remember, you should tap at whatever rate feels most <strong>natural and comfortable</strong> to you. Press the SPACEBAR ' +
            'when you are ready to begin.</p>'
    };

    // Dynamically adjust certain instructions based on condition
    if (tap_condition) {
        var instruction_text = '<p>In addition to listening to the sounds on each trial, you should also tap along with both the metronome and piano tones, ' +
            'trying to time your taps so that they land perfectly in sync with the start of each sound. If you are right handed, you should do this by tapping ' +
            'the J key with your right index finger. If you are left-handed, you should do this by tapping the F key with your left index finger.</p>';
        var post_practice_text = '<p>Remember, your goal is to determine how much faster or slower the repeating tone is, compared to the metronome, while you tap in time with each.</p>';
        var break_text = '<p><em>Remember to use the J or F key to tap in time with the clicks and tones on each trial.</em></p>';
        var trial_text = '<p>Remember to tap along while listening.</p>';
    } else {
        var instruction_text = '<p>While listening to the metronome and piano tones, try to keep your physical movements to a minimum, and avoid tapping or moving ' +
            'along with the sounds.</p>';
        var post_practice_text = '<p>Remember, your goal is to determine how much faster or slower the repeating tone is, compared to the metronome, while keeping your ' +
            'movements to a minimum.</p>';
        var break_text = '<p><em>Remember to avoid moving while listening to the clicks and tones on each trial.</em></p>';
        var trial_text = '<p>Remember to avoid moving while listening.</p>';
    }

    // Main task instructions (pre-practice)
    var instructions_main = {
        type: 'html-button-response',
        data: { event: 'main_instructions' },
        choices: ['Start'],
        post_trial_gap: post_instruction_delay,
        stimulus: '<p>Now you are ready to proceed to the main task.</p><p>On each trial, you will hear a series of clicks from a metronome, followed by a ' +
            'repeating piano tone. Your goal is to determine how much faster or slower this repeating tone is, compared to the metronome.</p><p>After listening to ' +
            'each pair, you will be given a slider with which to respond. The slider will range from "Half as Fast" on the left to "Twice as Fast" on the right. ' +
            'If the repeating tone was slower than the metronome, you should move the slider to the left by a distance which represents how much slower it was. ' +
            'If the repeating tone was faster than the metronome, you should instead move the slider to the right by a distance which represents how much ' +
            'faster it was. If the repeating tone was played at the same rate as the metronome, you should place the slider in the center of the range, where ' +
            'it is marked "Equal Rates".</p>' + instruction_text + '<p>Press the button below to try three practice trials, which will help you get comfortable with the task.</p>'
    };

    // Final main task instructions (post-practice)
    var instructions_final = {
        type: 'html-button-response',
        data: { event: 'summary_instructions' },
        choices: ['Start'],
        post_trial_gap: post_instruction_delay,
        stimulus: '<p>You have completed the practice trials and are now ready to begin!</p>' + post_practice_text + '<p>Please note that the trials are organized into three ' +
            'blocks, and you will have the opportunity to rest between blocks. If the practice trials were too loud or too quiet, adjust your volume now.</p><p><strong>' +
            'It is critical that you do not adjust your volume any further until the study has concluded.</strong></p><p>Press the button below when you are ready to proceed.</p>'
    };

    // First break screen (after Block 1)
    var break1 = {
        type: 'html-button-response',
        data: { event: 'break' },
        choices: ['Continue'],
        post_trial_gap: post_instruction_delay,
        stimulus: '<h3>You have completed block 1 of 3</h3><p>When you are ready to continue, press the button below to begin the second block.</p>' + break_text +
            '<p><em>Additionally, please do not adjust your volume until the study has concluded.</em></p>'
    };

    // Second break screen (after Block 2)
    var break2 = {
        type: 'html-button-response',
        data: { event: 'break' },
        choices: ['Continue'],
        post_trial_gap: post_instruction_delay,
        stimulus: '<h3>You have completed block 2 of 3</h3><p>When you are ready to continue, press the button below to begin the final block.</p>' + break_text +
            '<p><em>Additionally, please do not adjust your volume until the study has concluded.</em></p>'
    };

    // Completion screen
    var ending = {
        type: 'html-button-response',
        data: { event: 'ending' },
        choices: ['Submit'],
        stimulus: '<h3>You have completed Block 3 of 3</h3><p>Thank you for participating! When you are ready, press the button below to submit your data and complete the study.</p>' +
            '<p>You will receive credit on SONA once we verify that you have completed our study.</p><p>If you have any questions or would like to learn more about ' +
            'our lab\'s work, please contact this study\'s organizer, Jesse Pazdera, at pazderaj@mcmaster.ca.</p><p><em>Have a great day!</em></p>'
    };

    /* - - - - PRE-TASKS - - - - */

    // Audio test sound player
    var sound_test = {
        type: 'audio-keyboard-response',
        data: { event: 'headphone_test_tones' },
        stimulus: jsPsych.timelineVariable('stimulus'),
        response_ends_trial: false,
        trial_ends_after_audio: true,
        prompt: '<h3>Sound check</h3><p>You should hear three tones in a sequence. Try to determine which tone is the <strong>quietest</strong>.</p>'
    };

    // Audio test response screen
    var sound_response = {
        type: 'html-keyboard-response',
        data: { event: 'headphone_test_response' },
        choices: ['1', '2', '3', '0'],
        post_trial_gap: post_response_delay,
        stimulus: '<p>Press the 1, 2, or 3 key on your keyboard to indicate which tone was the quietest.</p>' +
            '<p>If you were not able to hear three tones, check that your headphones are working and that your volume is turned up, then press 0.</p>'
    };

    // Audio test trial
    var sound_check_procedure = {
        timeline: [sound_test, sound_response],
        timeline_variables: [
            { stimulus: 'headphone_check/antiphase_HC_IOS.wav' },
            { stimulus: 'headphone_check/antiphase_HC_ISO.wav' },
            { stimulus: 'headphone_check/antiphase_HC_OIS.wav' },
            { stimulus: 'headphone_check/antiphase_HC_OSI.wav' },
            { stimulus: 'headphone_check/antiphase_HC_SIO.wav' },
            { stimulus: 'headphone_check/antiphase_HC_SOI.wav' },
        ],
        randomize_order: true,
        repetitions: 1
    };

    // Preferred rate test
    var tapping_test = {
        type: 'audio-multiple-response',
        data: { event: 'tapping_test' },
        stimulus: '',
        prompt: '<h1>+</h1>',
        trial_ends_after_audio: false,
        trial_duration: tap_test_duration,
        post_trial_gap: post_response_delay
    };

    /* - - - - MAIN TASK - - - - */

    // Reference sequence
    var reference = {
        type: 'audio-multiple-response',
        data: { event: 'metronome' },
        prompt: trial_text,
        stimulus: 'stimuli/reference_sequence.wav',
        response_ends_trial: false,
        trial_ends_after_audio: true,
        post_trial_gap: post_reference_delay
    };

    // First practice trial
    var practice1 = {
        type: 'audio-multiple-response',
        data: { event: 'practice_tones' },
        prompt: trial_text,
        stimulus: 'stimuli/practice_sequence_741.wav',
        response_ends_trial: false,
        trial_ends_after_audio: true
    };

    // Second practice trial
    var practice2 = {
        type: 'audio-multiple-response',
        data: { event: 'practice_tones' },
        prompt: trial_text,
        stimulus: 'stimuli/practice_sequence_550.wav',
        response_ends_trial: false,
        trial_ends_after_audio: true
    };

    // Third practice trial
    var practice3 = {
        type: 'audio-multiple-response',
        data: { event: 'practice_tones' },
        prompt: trial_text,
        stimulus: 'stimuli/practice_sequence_407.wav',
        response_ends_trial: false,
        trial_ends_after_audio: true
    };

    // Response slider
    var response = {
        type: 'html-slider-response',
        data: { event: 'response' },
        stimulus: ['How fast was the repeating tone relative to the metronome?'],
        labels: ['Half as Fast', 'Equal Rates', 'Twice as Fast'],
        require_movement: true,
        post_trial_gap: post_response_delay
    };

    /* - - - - BLOCKING - - - - */

    // Start building up list of audio files that will need to be loaded
    var audio_files = ['stimuli/reference_sequence.wav'];
    var headphone_check_strings = ['IOS', 'ISO', 'OIS', 'OSI', 'SIO', 'SOI'];
    for (i in headphone_check_strings) {
        s = headphone_check_strings[i];
        audio_files.push(`headphone_check/antiphase_HC_${s}.wav`);
    }

    // Add instructions, pre-tests, and practice trials to timeline
    var timeline = [pavlovia_init];
    timeline = timeline.concat([welcome, instructions_requirements]);  // Starting info
    timeline = timeline.concat([instructions_audio_test, sound_check_procedure]);  // Audio test
    timeline = timeline.concat([instructions_tapping_test, tapping_test]);  // Tapping test
    timeline = timeline.concat([instructions_main, reference, practice1, response, reference, practice2, response, reference, practice3, response, instructions_final]);  // Task info and practice

    // Dynamically construct trials based on schedule, while adding each audio file to the preload list
    for (block = 0; block < 3; block++) {
        for (trial = 0; trial < trials_per_block; trial++) {
            audio_files.push(`stimuli/sequence_${schedule[block][trial][0]}_${schedule[block][trial][1]}_${schedule[block][trial][2]}.wav`)
            timeline.push(reference)
            timeline.push({
                type: 'audio-multiple-response',
                data: { event: 'tones' },
                prompt: trial_text,
                stimulus: `stimuli/sequence_${schedule[block][trial][0]}_${schedule[block][trial][1]}_${schedule[block][trial][2]}.wav`,
                response_ends_trial: false,
                trial_ends_after_audio: true
            });
            timeline.push(response);
        }
        if (block == 0) {
            timeline.push(break1);
        } else if (block == 1) {
            timeline.push(break2);
        }
    }

    // Set up experiment conclusion
    timeline.push(ending);
    timeline.push(pavlovia_finish);

    /* - - - - EXECUTION - - - - */

    jsPsych.init({
        timeline: timeline,
        default_iti: 0,
        use_webaudio: true,
        preload_audio: audio_files,
        show_preload_progress_bar: true,
        show_progress_bar: true,
        exclusions: {audio: true}
        //on_finish: function () {
        //jsPsych.data.displayData('csv');
        //jsPsych.data.get().localSave('csv', `data${subjectID}.csv`)
        //}
    });
});