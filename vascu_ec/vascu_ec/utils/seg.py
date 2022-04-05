from pathlib import Path

import cellpose.models
import numpy as np
import skimage.segmentation
from scipy import ndimage as ndi

from vascu_ec.vascu_ec_logging import get_logger


def get_cellpose_model(use_gpu):
    """Gets the cellpose default model"""
    return cellpose.models.Cellpose(gpu=use_gpu, model_type='cyto')


def get_cellpose_segmentation(parameters, im_seg):
    """Gets the cellpose segmentation"""
    get_logger().info("Calculate cellpose segmentation. This might take some time...")

    model = get_cellpose_model(parameters["use_gpu"])
    if parameters["channel_nucleus"] >= 0:
        channels = [1, 2]
    else:
        channels = [0, 0]

    # masks, flows, styles, diams = model.eval(im_seg, channels=channels)
    masks, flows, styles, diams = model.eval(im_seg, diameter=parameters["estimated_cell_diameter"], channels=channels)

    return masks


def load_or_get_cellpose_segmentation(parameters, img_seg, filepath):
    get_logger().info("Look up cellpose segmentation...")
    stem = Path(filepath).stem
    segmentation = Path(filepath).parent.joinpath(stem + "_seg.npy")

    if segmentation.exists():
        get_logger().info("Load cellpose segmentation...")

        # in case an annotated mask is available
        cellpose_seg = np.load(str(segmentation), allow_pickle=True)
        cellpose_mask = cellpose_seg.item()['masks']

    else:
        cellpose_mask = get_cellpose_segmentation(parameters, img_seg)

    if parameters["clear_border"]:
        cellpose_mask_clear_border = skimage.segmentation.clear_border(cellpose_mask)
        number_of_cellpose_borders = len(np.unique(cellpose_mask)) - len(np.unique(cellpose_mask_clear_border))
        cellpose_mask = cellpose_mask_clear_border

        get_logger().info("Removed number of cellpose borders: %s" % number_of_cellpose_borders)

    get_logger().info("Detected number of cellpose labels: %s" % len(np.unique(cellpose_mask)))

    return cellpose_mask


def get_outline_from_mask(mask, width=1):
    """"""
    # TODO: revise use built in function from cellpose

    mask = mask.astype(bool)
    dilated_mask = ndi.binary_dilation(mask, iterations=width)
    eroded_mask = ndi.binary_erosion(mask, iterations=width)
    outline_mask = np.logical_xor(dilated_mask, eroded_mask)

    return outline_mask
