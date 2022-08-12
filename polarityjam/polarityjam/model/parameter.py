import os
from pathlib import Path

from polarityjam.utils.io import read_parameters


class Parameter:

    def __init__(self, *args, **kwargs):
        # init with default values from resources
        current_path = Path(os.path.dirname(os.path.realpath(__file__)))
        param_base_file = Path(current_path).joinpath("..", "utils", "resources", "parameters.yml")
        args_init = read_parameters(str(param_base_file))

        for key in args_init:
            self._setattr(key, args_init[key])

        if args != ():
            for dictionary in args:
                for key in dictionary:
                    self._setattr(key, dictionary[key])

        if kwargs != {}:
            for key in kwargs:
                self._setattr(key, kwargs[key])

    def _setattr(self, key, val):
        if hasattr(self, key):
            setattr(self, key, val)

    def reset(self):
        for key in self.__dict__:
            self._setattr(key, None)

    @classmethod
    def from_yml(cls, path):
        params = read_parameters(path)

        cls(**params)

    def __str__(self, indent=1):
        s = '%s  \n' % self.__class__.__name__
        for attr in self.__dict__:
            for i in range(0, indent):
                s += '\t'
            s += (attr + ':\t' + str(getattr(self, attr))) + '\n'
        return s


class SegmentationParameter(Parameter):

    def __init__(self, attrs=None):
        if attrs is None:
            attrs = {}
        self.manually_annotated_mask = None
        self.store_segmentation = None
        self.use_given_mask = None
        self.model_type = None
        self.model_path = None
        self.estimated_cell_diameter = None
        self.use_gpu = None
        self.clear_border = None
        self.min_cell_size = None

        super().__init__(**attrs)


class ImageParameter(Parameter):

    def __init__(self, attrs=None):
        if attrs is None:
            attrs = {}
        self.channel_junction = None
        self.channel_nucleus = None
        self.channel_organelle = None
        self.channel_expression_marker = None

        super().__init__(**attrs)

    def __len__(self):
        c = 0
        for key in self.__dict__:
            if getattr(self, key) is not None:
                c += 1

        return c


class InputParameter(Parameter):

    def __init__(self, attrs=None):
        if attrs is None:
            attrs = {}
        self.channel_junction = None
        self.channel_nucleus = None
        self.channel_organelle = None
        self.channel_expression_marker = None
        self.membrane_thickness = None
        self.feature_of_interest = None
        self.min_cell_size = None
        self.min_nucleus_size = None
        self.min_organelle_size = None
        self.dp_epsilon = None

        super().__init__(**attrs)


class PlotParameter(Parameter):

    def __init__(self, attrs=None):
        if attrs is None:
            attrs = {}
        self.plot_junctions = None
        self.plot_polarity = None
        self.plot_orientation = None
        self.plot_marker = None
        self.plot_ratio_method = None
        self.plot_cyclic_orientation = None

        self.outline_width = None
        self.show_polarity_angles = None
        self.show_graphics_axis = None
        self.pixel_to_micron_ratio = None
        self.plot_scalebar = None
        self.length_scalebar_microns = None

        self.graphics_output_format = None
        self.dpi = None
        self.graphics_width = None
        self.graphics_height = None
        self.membrane_thickness = None  # todo: remove me

        super().__init__(**attrs)
