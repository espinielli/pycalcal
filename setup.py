import setuptools

with open("Readme.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="pycalcal",
    version="0.0.1",
    description="Minimal package for using pycalcal.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/rn123/pycalcal",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
)
