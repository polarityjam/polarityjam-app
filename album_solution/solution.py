from album.runner.api import setup

glob_ip = ""

env_file = """name: Polarity-JaM-App
channels:
  - conda-forge
  - defaults
  - r
dependencies:
  - python=3.8
  - pip
  - git=2.34
  - r-base=4.0.5
  - r-shiny=1.6.0
  - r-shinyfiles=0.9.0
  - r-shinycssloaders=1.0.0
  - r-shinyWidgets=0.6.0
  - r-optparse=1.7.1
  - r-ggplot2=3.3.3
  - r-tidyverse=1.3.0
  - r-circular=0.4_93
  - r-gridextra=2.3
  - r-circstats=0.2_6
  - r-readxl=1.3.1
  - r-jsonlite=1.7.2
  - r-rjson=0.2.20
  - pip:
    - GitPython==3.1
    - rpy2
  
"""


def install():
    from album.runner.api import get_package_path
    from git import Repo
    from pathlib import Path
    import os
    print("Installing the Polarity-JaM app")
    polarityjam_repo = Path(get_package_path()).joinpath('polarityjam_app')
    os.mkdir(polarityjam_repo)
    Repo.clone_from("https://github.com/polarityjam/polarityjam-app.git", polarityjam_repo)

    # install pacman via CRAN - package mandatory, but not in R channel available
    from rpy2.robjects.packages import importr
    utils = importr('utils')
    utils.install_packages('pacman', repos="https://cloud.r-project.org")


def run():
    from album.runner.api import get_package_path
    from io import StringIO
    import os
    import sys
    from pathlib import Path
    from subprocess import Popen
    if sys.platform == "win32":
        from subprocess import CREATE_NEW_CONSOLE
    else:
        CREATE_NEW_CONSOLE = 0
    import webbrowser
    import re
    import threading

    class LogPipe(threading.Thread):

        # Class adapted from GitHub: https://gist.github.com/alfredodeza/dcea71d5c0234c54d9b1

        def __init__(self):
            """Setup the object with a logger and a loglevel
            and start the thread
            """
            threading.Thread.__init__(self)
            self.daemon = False
            self.level = None
            self.fdRead, self.fdWrite = os.pipe()
            self.pipeReader = os.fdopen(self.fdRead)
            self.process = None
            self.start()
            self.buffer = StringIO()

        def fileno(self):
            """Return the write file descriptor of the pipe
            """
            return self.fdWrite

        def run(self):
            """Run the thread, logging everything.
            """
            for line in iter(self.pipeReader.readline, ''):
                self.buffer.write(line)
            self.pipeReader.close()

        def close(self):
            """Close the write end of the pipe.
            """
            os.close(self.fdWrite)

        def stop(self):
            self._stop = True
            self.close()

        def __del__(self):
            try:
                self.stop()
            except:
                pass
            try:
                del self.fdRead
                del self.fdWrite
            except:
                pass

    # Path to R-Shiny-App
    polarityjam_app_path = Path(get_package_path()).joinpath('polarityjam_app', 'app', 'app.R')
    os.chdir(str(polarityjam_app_path.parent.resolve()))

    # regular expression to filter R-Shiny output for the IP of the App
    global glob_ip
    regex_ip = re.compile(r'http://[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+:[0-9]+')

    # special Pipe to access its communicated values
    logpipe = LogPipe()

    # start the R-Shiny-App in a subprocess
    process = Popen(
        ["Rscript", str(polarityjam_app_path)],
        creationflags=CREATE_NEW_CONSOLE,
        stdout=logpipe,
        stderr=logpipe
    )

    # While the subprocess runs the output gets filtered for the Ip of the App. If found a webbrowser will be opened
    while process.poll() == None:
        tmp_str = str(logpipe.buffer.getvalue())
        regex_match = regex_ip.search(tmp_str)
        if regex_match:
            print(tmp_str)  # print all we got so far
            glob_ip = regex_match.group()
            webbrowser.open(glob_ip, new=1)
            break

    # case error during starting
    if process.poll() != None:  # process should not have terminated by now
        print(str(logpipe.buffer.getvalue()))  # print the error

    # close special pipe
    logpipe.close()

    # report as normal
    for line in iter(logpipe.pipeReader.readline, ''):
        print(line)


def prepare_test():
    return {}


def test():
    import urllib.request
    global glob_ip
    # Test if the App is live
    if urllib.request.urlopen(glob_ip).getcode() == 200:
        print("Polarityjam R-shiny App succesfully started. Test succesfull!")
    else:
        print("Polarityjam R-shiny App could not be started. Test failed!")


setup(
    group="de.mdc-berlin",
    name="polarityjam-app",
    version="0.1.0",
    title="Polarityjam R-Shiny App Solution",
    description="A Solution to run the Polarityjam R Shiny App.",
    solution_creators=["Lucas Rieckert", "Jan Philipp Albrecht"],
    cite=[{
        "text": "Polarity-JaM: An image analysis toolbox for cell polarity, junction and morphology quantification",
        "doi": "10.1101/2024.01.24.577027"
    }],
    tags=[
        "polarityjam",
        "cell polarity",
        "circular statistics",
        "endothelial cells",
        "workflow",
        "software",
        "shiny"
    ],
    license="MIT",
    documentation=["doc.md"],
    covers=[{"description": "Polarityjam app cover image", "source": "cover.png"}],
    album_api_version="0.5.5",
    install=install,
    run=run,
    pre_test=prepare_test,
    test=test,
    dependencies={'environment_file': env_file}
)
