Installation
============


Feature Extraction Pipeline
---------------------------

The feature extraction pipeline is the process of extracting all relevant features from all input ima0ges.
The result will always be a csv file containing for each image its individual cells as rows and their
corresponding feature values as columns. Additionally, plots will be created. Depending on the run
configuration these visualizations can be used for quality control but might also be suitable for a
publication. Look at the Output section to get to know more.

Automatic installation with album
+++++++++++++++++++++++++++++++++

This paragraph will be updated shortly.


Manual installation
+++++++++++++++++++
Execute the following steps to manually install the polarityjam feature extraction pipeline:

.. code-block:: console

    git clone https://github.com/wgiese/polarityjam # via git or download via browser
    cd polarityjam/polarityjam
    pip install -e polarityjam


.. tip::

    It is strongly recommended to have a working `conda <https://anaconda.com/>`_ installation and to operate in an activated environment!


Web App
-------------------

The R-shiny web app further analyses the results of the feature extraction process in the browser.
There are several statistics available which parameters can be adapted during runtime to immediately
observe the change in the corresponding visualization.
Thus, Exploring the data and relieving interesting patterns is heavily facilitated.
To get to know more about the statics continue reading or visit the :any:`Methods <Methods>` section.



Automatic installation with album
+++++++++++++++++++++++++++++++++

This paragraph will be updated shortly.

Manual installation
+++++++++++++++++++

Make sure you have `conda <https://anaconda.com/>`_ installed.

Clone the repository and then execute the following steps on the commandline:

.. code-block:: console

    cd polarityjam
    conda env create -f polarityjam.yml
    conda activate polarityjam
    Rscript app/app.R

Open the browser in the URL given in the output of the R-shiny call. (Usually smth. like http://127.0.0.1:6660)