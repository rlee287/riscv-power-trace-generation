use hdf5::{Dataset, H5Type};

use itertools::Itertools;

use crate::HDF5_CHUNK_SIZE;

pub fn write_iter_to_dataset<T: H5Type>(dataset: &Dataset, src: impl IntoIterator<Item = T>) {
    for (i, chunk) in src.into_iter().chunks(HDF5_CHUNK_SIZE).into_iter().enumerate() {
        let chunk_data: Vec<T> = chunk.collect();
        let start_idx = i*HDF5_CHUNK_SIZE;
        dataset.resize(start_idx+chunk_data.len()).unwrap();
        match chunk_data.len() {
            HDF5_CHUNK_SIZE => {
                dataset.write_slice(&chunk_data, start_idx..(i+1)*HDF5_CHUNK_SIZE).unwrap();
            },
            _ => {
                dataset.write_slice(&chunk_data, start_idx..).unwrap();
            }
        }
    }
}