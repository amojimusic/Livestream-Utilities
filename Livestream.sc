Livestream {



	classvar kickName = \kick;
	classvar snareName = \snare;
	classvar bassName = \bass;
	classvar synthName = \synth;



	*boot {
		Server.local.waitForBoot;
	}

	*init {

		this.boot;

		SynthDef.new(kickName, {
			var env, sig;
			env = Env.perc(\atk.kr(0.01), \dec.kr(0.1), \amp.kr(1), \curve.kr(-4)).kr(2);
			sig = SinOsc.ar(\freq.kr(10) + (env * 100), 0, \mul.kr(1));
			sig = Pan2.ar(sig * env, \pan.kr(0));
			sig = sig * 0.5;
			sig = Clip.ar(sig, -0.5, 0.5);
			Out.ar(\out.kr(0), sig);
		}).add;

		SynthDef.new(snareName, {
			var env, sig, noise;
			noise = WhiteNoise.ar(\nmul.kr(1));
			env = Env.perc(\atk.kr(0.01), \dec.kr(0.1), \amp.kr(1), \curve.kr(-4)).kr(2);
			sig = SinOsc.ar(\freq.kr(350) + (env * 100) + (noise * 1000), 0, \mul.kr(1));
			sig = Pan2.ar(sig * env, \pan.kr(0));
			sig = sig * 0.5;
			sig = Clip.ar(sig, -0.5, 0.5);
			Out.ar(\out.kr(0), sig);
		}).add;

		SynthDef.new(bassName, {
			var env, sig;
			env = Env.perc(\atk.kr(0.01), \dec.kr(1), \amp.kr(1), \curve.kr(-4)).kr(2);
			sig = Saw.ar(\freq.kr(440), \mul.kr(1));
			sig = sig * env;
			sig = Pan2.ar(sig, \pan.kr(0));
			sig = sig * 0.5;
			sig = Clip.ar(sig, -0.5, 0.5);
			Out.ar(\out.kr(0), sig);
		}).add;

		SynthDef.new(synthName, {
			var env, sig;
			env = Env.perc(\atk.kr(0.01), \dec.kr(1), \amp.kr(1), \curve.kr(-4)).kr(2);
			sig = Saw.ar(\freq.kr(440), \mul.kr(1));
			sig = sig * env;
			sig = Pan2.ar(sig, \pan.kr(0));
			sig = sig * 0.5;
			sig = Clip.ar(sig, -0.5, 0.5);
			Out.ar(\out.kr(0), sig);
		}).add;



	}

	*kickName_ {arg name;
		kickName = name;
		this.init;
		^kickName;
	}

	*kickName {
		^kickName;
	}

	*snareName_ {arg name;
		snareName = name;
		this.init;
		^snareName;
	}

	*snareName {
		^snareName;
	}

	*bassName_ {arg name;
		bassName = name;
		this.init;
		^bassName;
	}

	*bassName {
		^bassName;
	}

	*synthName_ {arg name;
		synthName = name;
		this.init;
		^synthName;
	}

	*synthName {
		^synthName
	}

	*snare {

		^snareName;
	}

	*kick {

		^kickName;
	}

	*bass {
		^bassName;
	}

	*synth {
		^synthName;
	}

	*playKick {
		Synth(kickName);
	}

	*playSnare{
		Synth(snareName);
	}

	*playBass{arg freq = 440;
		Synth(bassName, [\freq, freq]);
	}

	*playSynth{arg freq = 440;
		Synth(synthName, [\freq, freq]);
	}

	*kickDef {arg func = {
			var env, sig;
			env = Env.perc(\atk.kr(0.01), \dec.kr(0.1), \amp.kr(1), \curve.kr(-4)).kr(2);
			sig = SinOsc.ar(\freq.kr(10) + (env * 100), 0, \mul.kr(1));
			sig = Pan2.ar(sig * env, \pan.kr(0));
			sig = sig * 0.5;
			sig = Clip.ar(sig, -0.5, 0.5);
			Out.ar(\out.kr(0), sig);
		};
		SynthDef.new(kickName, func).add;
	}

	*snareDef {arg func = {
			var env, sig, noise;
			noise = WhiteNoise.ar(\nmul.kr(1));
			env = Env.perc(\atk.kr(0.01), \dec.kr(0.1), \amp.kr(1), \curve.kr(-4)).kr(2);
			sig = SinOsc.ar(\freq.kr(350) + (env * 100) + (noise * 1000), 0, \mul.kr(1));
			sig = Pan2.ar(sig * env, \pan.kr(0));
			sig = sig * 0.5;
			sig = Clip.ar(sig, -0.5, 0.5);
			Out.ar(\out.kr(0), sig);
		};
		SynthDef.new(snareName, func).add;
	}

	*bassDef {arg func = {
			var env, sig;
			env = Env.perc(\atk.kr(0.01), \dec.kr(1), \amp.kr(1), \curve.kr(-4)).kr(2);
			sig = Saw.ar(\freq.kr(440), \mul.kr(1));
			sig = sig * env;
			sig = Pan2.ar(sig, \pan.kr(0));
			sig = sig * 0.5;
			sig = Clip.ar(sig, -0.5, 0.5);
			Out.ar(\out.kr(0), sig);
		};
		SynthDef.new(bassName, func).add;
	}

	*synthDef {arg func = {
			var env, sig;
			env = Env.perc(\atk.kr(0.01), \dec.kr(1), \amp.kr(1), \curve.kr(-4)).kr(2);
			sig = Saw.ar(\freq.kr(440), \mul.kr(1));
			sig = sig * env;
			sig = Pan2.ar(sig, \pan.kr(0));
			sig = sig * 0.5;
			sig = Clip.ar(sig, -0.5, 0.5);
			Out.ar(\out.kr(0), sig);
		};
		SynthDef.new(synthName, func).add;
	}


}
