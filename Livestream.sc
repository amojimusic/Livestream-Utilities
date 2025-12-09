Livestream {



	classvar kickName = \kick;
	classvar snareName = \snare;
	classvar bassName = \bass;
	classvar synthName = \synth;
	classvar hatName = \hat;
	classvar reverbName = \reverb;
	classvar delayName = \delay;
	classvar init = false;
	classvar reverbBus = \bus;
	classvar delayBus = \bus;
	classvar sig = \sig;




	*boot {
		Server.local.waitForBoot;
	}

	*clear {
		kickName = \kick;
	snareName = \snare;
	bassName = \bass;
	synthName = \synth;
	hatName = \hat;
	reverbName = \reverb;
	delayName = \delay;
		if(init == false){
	init = false;
		};
	reverbBus = \bus;
	delayBus = \bus;
	}

	*init {


		init = true;
		this.boot;
		reverbBus = Bus.audio(Server.local, 2);
		delayBus = Bus.audio(Server.local, 2);



		(SynthDef.new(hatName, {
	var env, sig, noise;
	env = Env.perc(\atk.kr(0.001), \dec.kr(0.5), \amp.kr(1), \curve.kr(-4)).kr(2);
	noise = RHPF.ar(WhiteNoise.ar(\nmul.kr(1)), 8000, 0.6);
	sig = [Pulse.ar(280 + SinOsc.ar(800, 0, 4) + (LocalIn.ar * 70 * \nmul.kr) + (noise * 15), 0.5, \mul.kr(1)), Pulse.ar(284.5 + SinOsc.ar(301, 0, 12) + (LocalIn.ar * 90 * \nmul.kr) + (noise * 15), 0.5, \mul.kr)].sum;
	sig.blend(noise, 0.2);
	LocalOut.ar(sig);
	sig = RHPF.ar(sig, \hp.kr(4900), \q.kr(1));
	sig = HPF.ar(sig, 11000).blend(sig, 0.27);
	sig = sig.blend(sig.softclip, 0.5);
	sig = FreeVerb.ar(sig, 0.1, 0.3, 0.5);
	sig = sig * env;

	sig = Pan2.ar(sig, \pan.kr(0));
	sig = sig * 0.5;
	sig = Clip.ar(sig, -0.5, 0.5);
	Out.ar(\out.kr(0), sig);
}).add;
);

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

		Ndef(reverbName, {
			var in;
			in = In.ar(reverbBus, 2);
			in = FreeVerb.ar(in, \mix.kr(0.5), \room.kr(0.5), \damp.kr(0.5));
			in = Clip.ar(in, -0.5, 0.5);
		});

		Ndef(delayName, {
			var in, del;
			in = In.ar(delayBus, 2);
			del = DelayN.ar(in, \del.kr(1), \del.kr);
			in = in.blend(del, \mix.kr(0.5).clip2(0.99));
			del = DelayN.ar(in, \del.kr(1), \del.kr);
			in = in.blend(del, \mix.kr(0.5).clip2(0.99) * 0.5);
			del = DelayN.ar(in, \del.kr(1), \del.kr);
			in = in.blend(del, \mix.kr(0.5).clip2(0.99) * 0.25);
			del = DelayN.ar(in, \del.kr(1), \del.kr);
			in = in.blend(del, \mix.kr(0.5).clip2(0.99) * 0.125);
			in = Clip.ar(in, -0.5, 0.5);



		});
	}

	*nInit {
		if(init == false){
			this.init();
		};
	}

	*hatName {
		^hatName;
	}

	*hatName_ {arg name;
		hatName = name;
		this.init;
		^hatName;
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

	*hat {
		this.nInit;
		^hatName;
	}

	*snare {
		this.nInit;
		^snareName;
	}

	*kick {
		this.nInit;
		^kickName;
	}

	*bass {
		this.nInit;
		^bassName;
	}

	*synth {
		this.nInit;
		^synthName;
	}

	*playHat {
		this.nInit;
		Synth(hatName);
	}

	*playKick {
		Synth(kickName);
	}

	*playSnare{
		this.nInit;
		Synth(snareName);
	}

	*playBass{arg freq = 440;
		this.nInit;
		Synth(bassName, [\freq, freq]);
	}

	*playSynth{arg freq = 440;
		this.nInit;
		Synth(synthName, [\freq, freq]);
	}

	*hatDef {arg func = {
	var env, sig, noise;
	env = Env.perc(\atk.kr(0.001), \dec.kr(0.5), \amp.kr(1), \curve.kr(-4)).kr(2);
	noise = RHPF.ar(WhiteNoise.ar(\nmul.kr(1)), 8000, 0.6);
	sig = [Pulse.ar(280 + SinOsc.ar(800, 0, 4) + (LocalIn.ar * 70 * \nmul.kr) + (noise * 15), 0.5, \mul.kr(1)), Pulse.ar(284.5 + SinOsc.ar(301, 0, 12) + (LocalIn.ar * 90 * \nmul.kr) + (noise * 15), 0.5, \mul.kr)].sum;
	sig.blend(noise, 0.2);
	LocalOut.ar(sig);
	sig = RHPF.ar(sig, \hp.kr(4900), \q.kr(1));
	sig = HPF.ar(sig, 11000).blend(sig, 0.27);
	sig = sig.blend(sig.softclip, 0.5);
	sig = FreeVerb.ar(sig, 0.1, 0.3, 0.5);
	sig = sig * env;

	sig = Pan2.ar(sig, \pan.kr(0));
	sig = sig * 0.5;
	sig = Clip.ar(sig, -0.5, 0.5);
	Out.ar(\out.kr(0), sig);
};
	this.nInit;
		SynthDef.new(\hatName, func);
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
	this.nInit;
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
	this.nInit;
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
	this.nInit;
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
	this.nInit;
		SynthDef.new(synthName, func).add;
	}

	*reverbName {
		^reverbName;
	}

	*reverb {
		this.nInit;
		^reverbName;
	}

	*playReverb {
		this.nInit;
		Ndef(reverbName).play;
	}

	*reverbBus {
		^reverbBus
	}

	*reverbBus_ {arg bus;
		reverbBus = bus;
	}

	*reverbDef {arg func;
		Ndef(reverbName, func);
	}

	*delayName {
		^delayName;
	}

	*delay {
		this.nInit;
		^delayName;
	}

	*playDelay {
		this.nInit;
		Ndef(delayName).play;
	}

	*delayBus {
		^delayBus
	}

	*delayBus_ {arg bus;
		delayBus = bus;
	}

	*delayDef {arg func;
		Ndef(delayName, func);
	}

}

Livestream34 : Livestream {

	*play{
		this.define;

		(~pat = {
			~tonic = [60, 59, 58, 61, 62].choose;
			~di = 12/19;

			~tuning = [~tonic, ~tonic + ~di, ~tonic + (~di * 2), ~tonic + (~di * 3), ~tonic + (~di * 4), ~tonic + (~di * 5), ~tonic + (~di * 6), ~tonic + (~di * 7), ~tonic + (~di * 8), ~tonic + (~di * 9), ~tonic + (~di * 10), ~tonic + (~di * 11), ~tonic + (~di * 12), ~tonic + (~di * 13), ~tonic + (~di * 14), ~tonic + (~di * 15), ~tonic + (~di * 16), ~tonic + (~di * 17), ~tonic + (~di * 18)].midicps;

			~scale = [~tuning[0], ~tuning[3], ~tuning[6], ~tuning[8], ~tuning[11], ~tuning[12], ~tuning[17]];

			~fr = rrand(0.8, 1.8);
			~tr = rrand(0.55, 1.3);

			Livestream.delayDef({
				var in, del;
				in = In.ar(Livestream.delayBus, 2);
				del = DelayN.ar(in, 0.25 * ~tr, 0.125 * ~tr);
				in = in.blend(del, 0.2);
				del = DelayN.ar(in, 0.25 * ~tr, 0.25 * ~tr);
				in = in.blend(del, 0.16);
				del = DelayN.ar(in, 0.25 * ~tr, 0.125 * ~tr);
				in = in.blend(del, 0.12);
				del = DelayN.ar(in, 0.25 * ~tr, 0.25 * ~tr);
				in = in.blend(del, 0.08);
				del = DelayN.ar(in, 0.5 * ~tr, 0.5 * ~tr);
				in = in.blend(del, 0.08);
				del = DelayN.ar(in, 0.5 * ~tr, 0.5 * ~tr);
				in = in.blend(del, 0.08);
				in = FreeVerb.ar(in, 0.1, 0.2, 1);
			});

			Pdef(\drums, Pbind(
				\instrument, Pseq([Curt.kick1, Curt.hat1, Curt.kick1, Curt.any, Livestream.snare, Curt.any, Curt.any], inf),
				\dur, Prand([Pseq([1], 3), Pseq([0.5], 6), Pseq([0.5, 0.25, 0.25, 0.75, 0.125, 0.125, 0.25, 0.25, 0.25, 0.125, 0.125], 2)], inf) * ~tr * [0.25, 0.5, 1].choose,
				\freq, Prand(~scale, inf) * Prand([0.05, 0.01, 0.1, 0.2, 0.5, 1, 2], inf) * ~fr,
				\amp, rrand(0.8, 1.1),
				\mul, Pwhite(0.05, 0.15, inf) * 0.3,
				\atk, Pwhite(0.0000001, 0.01),
				\dec, Pwhite(rrand(0.02, 0.03), rrand(0.1, 0.25), inf),
				\curve, rrand(-10.0, 10.0),
				\pan, Pwhite(-0.3, 0.3, inf),
				\in, Pwhite(0.0, 300.0, inf) * [0.5, 1, 2, 4, 8].choose,
				\out, Pseq([0, 0, 0, Livestream.delayBus, 0, Livestream.reverbBus, 0, 0, Prand([0, Livestream.delayBus], Prand([1, 2, 3, 4, 5], 1)), 0], inf)
			));

			Pdef(\chords, Pbind(
				\instrument, \synth,
				\dur, 4 * ~tr,
				\atk, 3 * ~tr,
				\dec, 2.5 * ~tr,
				\amp, 0.1,
				\mul, 0.06,
				\lp, 2700,
				\lq, 0.75,
				\hp, 800,
				\hq, 0.4,
				\pan, Pwhite(-0.2, 0.2, inf),
				\freq, Prand([[~scale[0], ~scale[2], ~scale[4]], [~scale[0], ~scale[2], ~scale[4]], [~scale[1], ~scale[3], ~scale[5]], [~scale[2], ~scale[4], ~scale[6]], [~scale[0], ~scale[1], ~scale[6]], [~scale[3], ~scale[1], ~scale[5]]], inf) * Prand([1, 2, 4], inf) * ~fr,
				\out, Livestream.reverbBus
			));

			(Pdef(\bass, Pbind(
				\instrument, \synth,
				\dur, Pseq([[Pseq([0.25], 8), Pseq([0.5], 4), Pseq([1], 2), 2].choose, [Pseq([0.25], 4), Pseq([0.5], 2), 1].choose, 1, 0.5, 0.5, 0.5, 0.25, 0.25], inf) * 2 * ~tr,
				\atk, rrand(0.05, 0.15),
				\dec, 1.3 * 2,
				\amp, 0.4,
				\mul, 0.3,
				\freq, Pseq([~scale[0], ~scale[0], ~scale[3], ~scale[1], ~scale[2], Prand(~scale, 2)], inf) * Prand([0.125, 0.125 * 0.5, 0.125 * 0.5, 0.125, 0.125, 0.125, 0.25, 0.25, 0.125, 0.125, 0.125], inf) * ~fr,
				\lp, Pwhite(200, 1000),
				\lq, Pwhite(0.8, 1.0, inf),
				\hp, 2,
				\pan, Pwhite(-0.1, 0.1, inf),
				\out, Prand([0, Livestream.delayBus, 0, Livestream.reverbBus, 0, 0, 0], inf)
			)));




		});

		Task({ loop{

			~pat.();
			wait([16, 32, 64].choose * ~tr);
		}}).play;



		Task({
			wait(7);

			Livestream.playReverb;



			Pdef(\drums).play;
			Pdef(\chords).play;
			Pdef(\bass).play;

		}).play;


	}

	*stop {
		Pdef(\drums).stop;
		Pdef(\chords).stop;
		Pdef(\bass).stop;
	}

	*define{

		Livestream.nInit;
		Task({
			wait(5);
			Curt.init;
			SynthDef.new(\synth, {
				var sig, env;
				sig = GeneRand.ar(\freq.kr(440) + (LocalIn.ar * 10)) * \mul.kr(1);
				env = Env.perc(\atk.kr(0.01), \dec.kr(1), \amp.kr(1), \curve.kr(-4.0)).kr(2);
				sig = sig * env;
				sig = RLPF.ar(sig, \lp.kr(3000), \lq.kr(1));
				sig = RHPF.ar(sig, \hp.kr(100), \hq.kr(1));
				LocalOut.ar(sig);

				sig = Pan2.ar(sig, \pan.kr(0));
				sig = Clip.ar(sig, -0.5, 0.5);
				Out.ar(\out.kr(0), sig);
			}).add;

			Livestream.reverbDef({
				var in, del;
				in = In.ar(Livestream.reverbBus, 2);
				in = FreeVerb.ar(in, 0.8, 0.8, 0.9);
				del = DelayN.ar(in, 0.25 * ~tr, 0.125 * ~tr);
				in = in.blend(del, 0.2);
				del = DelayN.ar(in, 0.25 * ~tr, 0.25 * ~tr);
				in = in.blend(del, 0.16);
				del = DelayN.ar(in, 0.25 * ~tr, 0.125 * ~tr);
				in = in.blend(del, 0.12);
				del = DelayN.ar(in, 0.25 * ~tr, 0.25 * ~tr);
				in = in.blend(del, 0.08);
				del = DelayN.ar(in, 0.5 * ~tr, 0.5 * ~tr);
				in = in.blend(del, 0.08);
				del = DelayN.ar(in, 0.5 * ~tr, 0.5 * ~tr);
				in = in.blend(del, 0.08);
				in = FreeVerb.ar(in, 0.1, 0.2, 1);
			});
		}).play;
	}

	*pat {
		~pat.();
	}

}




