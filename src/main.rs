use comemo::Track;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

use crate::stream::{Env, Stream};

mod ast;
mod lexer;
mod parser;
mod resolve;
mod stream;

fn main() {
    let source = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let source = resolve::Source::new(source);
    let main = resolve::resolve("main", source.track());

    let (tx, rx) = std::sync::mpsc::sync_channel(24_000);

    let host = cpal::default_host();
    let device = host
        .default_output_device()
        .expect("no output device available");

    let config_range = device
        .supported_output_configs()
        .expect("error while querying configs")
        .next()
        .expect("no supported config?!");

    let config = config_range.with_sample_rate(cpal::SampleRate(24_000));

    let sample_format = config.sample_format();
    let print_err = move |err| println!("{}", err);
    let stream = match sample_format {
        cpal::SampleFormat::F64 => device.build_output_stream(
            &config.into(),
            move |data: &mut [f64], _: &cpal::OutputCallbackInfo| {
                for d in data {
                    *d = rx.recv().unwrap();
                }
            },
            print_err,
            None,
        ),
        cpal::SampleFormat::U8 => device.build_output_stream(
            &config.into(),
            move |data: &mut [u8], _: &cpal::OutputCallbackInfo| {
                for d in data {
                    let val = rx.recv().unwrap();
                    // dbg!(val);
                    *d = (val * (u8::MAX / 2) as f64) as u8;
                    // dbg!(d);
                }
            },
            print_err,
            None,
        ),
        x => panic!("unsupported format: {}", x),
    }
    .unwrap();

    stream.play().unwrap();

    match main {
        Some(main) => {
            dbg!(&main);
            let expr = main.all_expression().next().unwrap();
            let mut env = Env::empty();
            let mut time = 0;
            loop {
                match expr.eval(env.track_mut(), source.track(), time) {
                    Some(v) => {
                        // dbg!(v.0);
                        if tx.try_send(v.0).is_err() {
                            break;
                        }
                        time += 1;
                    }
                    None => {
                        println!("Done");
                        break;
                    }
                }
            }
        }
        None => println!("Error: please provide a main node"),
    }

    std::thread::sleep(std::time::Duration::from_secs(1));
}
