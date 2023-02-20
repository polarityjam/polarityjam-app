from album.runner.api import setup, get_args, get_package_path

glob_ip = ""

env_file = """name: Polarityjam-R-Shiny-App-Solution
channels:
  - conda-forge
  - defaults
  - r
dependencies:
  - python=3.8.13
  - pip
  - git=2.34
  - r-essentials=3.6
  - r-base=3.6
  - r-shinyfiles=0.9
  - r-circular=0.4
  - r-shape=1.4.6
  - r-shinyWidgets=0.4.9
  - r-gridExtra=2.3
  - r-FNN=1.1.3
  - r-CircStats=0.2.6
  - r-rjson=0.2.20
  - r-shinycssloaders=0.2.0
  - pip:
    - GitPython==3.1

"""


def install():
    from git import Repo
    from pathlib import Path
    import os
    print("Installing polarityjam and dependencies")
    polarityjam_repo = Path(get_package_path()).joinpath('polarityjam')
    os.mkdir(polarityjam_repo)
    Repo.clone_from("https://github.com/polarityjam/polarityjam-app.git", polarityjam_repo)


def run():
    from io import StringIO
    import os
    from pathlib import Path
    from subprocess import Popen, CREATE_NEW_CONSOLE
    import subprocess
    import platform
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
    polarityjam_repo = Path(get_package_path()).joinpath('polarityjam', 'app')
    tmp_str = ""

    # Pipe to pipe the output of the R-Shiny subprocess into a buffer StringIO which can be accessed
    logpipe = LogPipe()

    # regular expression to filter R-Shiny output for the IP of the App
    global glob_ip
    regex_ip = re.compile(r'http://[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+:[0-9]+')

    # start the R-Shiny-App in a subprocess
    if platform.system() == 'Windows':
        process = Popen(
            ["Rscript", str(Path(polarityjam_repo).joinpath('app.R'))],
            creationflags=CREATE_NEW_CONSOLE,
            stdout=logpipe, stderr=logpipe
        )
    else:
        process = subprocess.run(
            ["Rscript", str(Path(polarityjam_repo).joinpath('app.R'))], stdout=logpipe, stderr=logpipe, shell=True
        )

    # While the subprocess runs the output gets filtered for the Ip of the App. If found a browser will be opened
    while process.poll() is None:
        tmp_str = str(logpipe.buffer.getvalue())
        regex_match = regex_ip.search(tmp_str)
        if regex_match:
            glob_ip = regex_match.group()
            webbrowser.open(glob_ip, new=1)
            break
    logpipe.close()


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
    tags=["polarityjam", "r", "shiny", "app"],
    license="UNLICENSE",
    documentation=["doc.md"],
    covers=[{
        "description": "Polarityjam app cover image",
        "source": "cover.png"
    }],
    album_api_version="0.5.3",
    install=install,
    run=run,
    pre_test=prepare_test,
    test=test,
    dependencies={'environment_file': env_file}
)
