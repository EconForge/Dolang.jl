from setuptools import setup

setup(name='dolang',
      version='0.0.6',
      author=['Pablo Winant'],
      author_email='pablo.winant@gmail.com',
      packages=['dolang'],
      install_requires=['numpy', 'sympy', 'dataclasses', 'numba']
      )
