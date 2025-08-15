use nom::Err;
use clap::Parser;
use nom::bytes::complete::{tag, take};
use nom::combinator::{map, map_opt, map_res};
use nom::number::complete::{le_u32, le_u8};
use nom::IResult;
use regex::Regex;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use nom::error::ErrorKind;
use tinytga::{Bpp, RawPixel, RawTga};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;
use tokio::task::{JoinHandle, JoinSet};
use walkdir::WalkDir;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, help = "Directory to search input files in")]
    source_dir: String,

    #[arg(short, long, help = "Directory to save results in in")]
    destination_dir: String,

    #[arg(short, long, help = "Input TGA file in RGB565 format")]
    input_files_pattern: String,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let input_files_pattern = Regex::from_str(&args.input_files_pattern).unwrap();

    let paths = WalkDir::new(&args.source_dir)
        .into_iter()
        .filter_map(|entry| entry.ok().take_if(|e| e.path().is_file()))
        .map(|entry| entry.into_path())
        .filter(|path| input_files_pattern.is_match(path.to_str().unwrap()))
        .collect::<Vec<_>>();

    let conversion_futures = paths
        .into_iter()
        .map(move |path| {
            let relative_path = path.strip_prefix(&args.source_dir).unwrap();

            let mut path_buf = PathBuf::from(&args.destination_dir);
            path_buf.push(relative_path);
            path_buf.set_extension("png");
            tokio::spawn(async move { convert_file(path, path_buf).await })
        })
        .collect::<JoinSet<_>>();

    conversion_futures.join_all().await;

    //convert_file(args.input_file, args.output_file).await;
}

async fn convert_file(input_file: impl AsRef<Path>, output_file: impl AsRef<Path>) {
    let bytes = tokio::fs::read(input_file).await.unwrap();
    let source_image = RawTga::from_slice(bytes.as_slice()).unwrap();

    assert_eq!(source_image.color_bpp(), Bpp::Bits16);

    let (_, format) = read_extension(bytes.as_slice()).unwrap();

    let parent = output_file.as_ref().parent().unwrap();
    tokio::fs::create_dir_all(parent).await.unwrap();

    let file_future = File::create(output_file);

    let convert_future = match format {
        TxrFormat::Rgba4444 => convert_rgba4444(source_image),
        TxrFormat::Rgb565 => convert_rgb565(source_image),
        TxrFormat::Some8 => convert_rgb565(source_image), // Not clear how to transform, but anyway...
        _ => panic!()
    };
    let (file_result, convert_result) = tokio::join!(file_future, convert_future);

    let mut file = file_result.unwrap();

    let png_data = convert_result.unwrap();

    file.write_all(&png_data).await.unwrap();
}

fn convert_rgb565(source_image: RawTga) -> JoinHandle<Vec<u8>> {
    let rgb888_data = source_image
        .pixels()
        .map(|pixel| rgb565_to_888(&pixel).into())
        .flat_map(|(r, g, b)| [r, g, b])
        .collect::<Vec<_>>();
    let width = source_image.size().width;
    let height = source_image.size().height;

    let convert_future = tokio::task::spawn_blocking(move || {
        let mut png_data: Vec<u8> = Vec::new();
        // let writer = Cursor::new(png_data.as_mut_slice());
        let mut png_encoder = png::Encoder::new(&mut png_data, width, height);

        png_encoder.set_color(png::ColorType::Rgb);
        png_encoder.set_compression(png::Compression::Best);
        png_encoder.set_depth(png::BitDepth::Eight);
        let mut png_writer = png_encoder.write_header().unwrap();

        png_writer.write_image_data(rgb888_data.as_slice()).unwrap();
        png_writer.finish().unwrap();
        png_data
    });
    convert_future
}

fn convert_rgba4444(source_image: RawTga) -> JoinHandle<Vec<u8>> {
    let rgb888_data = source_image
        .pixels()
        .map(|pixel| rgb4444_to_8888(&pixel).into())
        .flat_map(|(r, g, b, a)| [r, g, b, a])
        .collect::<Vec<_>>();
    let width = source_image.size().width;
    let height = source_image.size().height;


    let convert_future = tokio::task::spawn_blocking(move || {
        let mut png_data: Vec<u8> = Vec::new();
        // let writer = Cursor::new(png_data.as_mut_slice());
        let mut png_encoder = png::Encoder::new(&mut png_data, width, height);

        png_encoder.set_color(png::ColorType::Rgba);
        png_encoder.set_compression(png::Compression::Best);
        png_encoder.set_depth(png::BitDepth::Eight);
        let mut png_writer = png_encoder.write_header().unwrap();

        png_writer.write_image_data(rgb888_data.as_slice()).unwrap();
        png_writer.finish().unwrap();
        png_data
    });
    convert_future
}

#[derive(Debug)]
enum TxrError {
    IOError,
    ParseError,
}

impl Display for TxrError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TxrError::IOError => f.write_str("IO error"),
            TxrError::ParseError => f.write_str("Parsing error"),
        }
    }
}

fn read_extension(input: &[u8]) -> IResult<&[u8], TxrFormat> {
    let (_input, extension_offset) = obtain_extension_offset(input)?;

    let (input, format) = obtain_format_from_extension(&input[extension_offset as usize..])?;

    Ok((input, format))
}

fn obtain_extension_offset(input: &[u8]) -> IResult<&[u8], u32> {
    let (input, id_len) = le_u8(input)?;
    let (input, _header) = take(17u32)(input)?;
    let (input, _magic_loff) = tag("LOFF")(input)?;
    // let (input, _magic_loff) = map_res(take(4u32), parse_assert_loff_magic)(input)?;
    let (input, _version_4) = map_res(
        |s| le_u32(s),
        |v| {
            if v == 4 {
                Ok(())
            } else {
                Err(TxrError::ParseError)
            }
        },
    )(input)?;

    let (input, offset) = le_u32(input)?;
    Ok((input, offset))
}

enum ExtensionTag {
    PFRM, // Format related data
    LVLS, // Some mip-levels related data
    LVMP, // Similar
    ENDR, // End marker
}

enum TxrFormat {
    // The values are from original
    Rgb565 = 2,
    Some3 = 3,
    Rgba4444 = 5,
    Some6 = 6,
    Some7 = 7,
    Some8 = 8, // Used by bumpcoord
}


fn obtain_format_from_extension(input: &[u8]) -> IResult<&[u8], TxrFormat> {

    let section_header_parser = |input| {
        let (input, tag) = map_res(take(4u32), parse_extension_tag)(input)?;
        let (input, section_size) = le_u32(input)?;

        Ok((input, (tag, section_size)))
    };

    let section_parser = |input| {

        let (input, (tag, section_size)) = section_header_parser(input)?;
        let section_beginning_input = input;

        let format = match tag {
            ExtensionTag::PFRM => map(
                |s| {
                    let (input, v0) = le_u32(input)?;
                    let (input, v1) = le_u32(input)?;
                    let (input, v2) = le_u32(input)?;
                    let (input, v3) = le_u32(input)?;

                    Ok((input, (v0, v1, v2, v3)))
                },
                |tuple| {
                    Some(match tuple {
                        (31744, 992, 31, 0) => TxrFormat::Some3,
                        (63488, 2016, 31, 0) => TxrFormat::Rgb565,
                        (3840, 240, 15, 61440) => TxrFormat::Rgba4444,
                        (16711680, 65280, 255, 0) => TxrFormat::Some6,
                        (16711680, 65280, 255, _) => TxrFormat::Some7,
                        (_, _, _, _) => TxrFormat::Some8,
                    })
                },
            )(input)?.1,
            ExtensionTag::ENDR => {
                return Err(Err::Error(nom::error::Error::new(input, ErrorKind::NoneOf)))
            }
            _ => None,
        };

        let rounded_up_section_size = (section_size + 3u32) & !3u32;
        let new_input = &section_beginning_input[rounded_up_section_size as usize..];

        Ok((new_input, format))
    };

    let mut loop_input = input.into();

    let (input, format) = loop {
        let (input, format) = section_parser(loop_input)?;
        loop_input = input;
        match format {
            None => {}
            Some(format) => { break (input, format) }
        };
    };

    Ok((input, format))
}

fn parse_assert_loff_magic(magic: &[u8]) -> Result<(), TxrError> {
    if magic != b"LOFF" {
        return Err(TxrError::ParseError);
    }
    Ok(())
}

fn parse_extension_tag(input: &[u8]) -> Result<ExtensionTag, TxrError> {
    Ok(match input {
        b"PFRM" => ExtensionTag::PFRM,
        b"LVLS" => ExtensionTag::LVLS,
        b"LVMP" => ExtensionTag::LVMP,
        b"ENDR" => ExtensionTag::ENDR,
        _ => return Err(TxrError::ParseError),
    })
}

fn rgb565_to_888(pixel: &RawPixel) -> (u8, u8, u8) {
    let b: u8 = ((pixel.color << 3) & 0b1111_1000) as u8;
    let g: u8 = ((pixel.color >> 3) & 0b1111_1100) as u8;
    let r: u8 = ((pixel.color >> 8) & 0b1111_1000) as u8;
    (r, g, b)
}

fn rgb4444_to_8888(pixel: &RawPixel) -> (u8, u8, u8, u8) {
    let b: u8 = ((pixel.color << 4) & 0b1111_0000) as u8;
    let g: u8 = ((pixel.color << 0) & 0b1111_0000) as u8;
    let r: u8 = ((pixel.color >> 4) & 0b1111_0000) as u8;
    let a: u8 = ((pixel.color >> 8) & 0b1111_0000) as u8;
    (r, g, b, a)
}
