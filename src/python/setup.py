from setuptools import setup, find_packages

setup(
    name="functorial-physics",
    version="0.1.0",
    author="Magneton Labs",
    author_email="info@magnetonlabs.com",
    description="Numerical simulations and algebraic tools for functorial physics",
    long_description=open("../../README.md").read(),
    long_description_content_type="text/markdown",
    url="https://github.com/MagnetonIO/functorial-physics",
    packages=find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Topic :: Scientific/Engineering :: Physics",
        "Topic :: Scientific/Engineering :: Mathematics",
    ],
    python_requires=">=3.10",
    install_requires=[
        "numpy>=1.24.0",
        "scipy>=1.10.0",
        "sympy>=1.12",
        "matplotlib>=3.7.0",
        "pandas>=2.0.0",
        "networkx>=3.0",
    ],
)