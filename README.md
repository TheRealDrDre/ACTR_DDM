# Comparing ACT-R to DDM

A repository of code for a comparison of Ratcliff's Drift-Diffusion
Model to an ACT-R model of choice. The ACT-R model implements a simple
version of a one-stimulus, two-alternative forced choice (2AFC) task.

## 2AFC in DDM

In DDM, decisions are modeled as drifting processes that proceed
towards one of two possible thresholds (corresponding to the
two options). The model is controlled by three parameters:
  1.  A drift parameter _v_
  2.  A threshold or boundary parameter _a_, which represents
      the distance from either of the response thresholds.
  3.  A non-decision time _T_, which represents response and
      encoding times.

## 2AFC in ACT-R

In ACT-R, decisions can be made through several mechanisms,
procedural and declarative. Here, we describe a general enough
mechanisms that comprises both, and is based on the following
steps:
  1.  The model holds in mind an internal correctness criterion
      in the working memory (WM) buffer, and maintains the
      two options in the visual buffer.
  2.  The model retrieves the S/R mappings from LTM.
  3.  The model proceeds through a check stage, during which
      it compares the retrieved S/R against the internal
      correctness criterion.
      1.  If a match is found, the model responds.
      2.  If not, the model either...
           1.  Restarts the search with probability _p_
           2.  Responds anyway with probability (1 - _p_).

### ACT-R Parameters

The main parameters in the ACT-R implementation are:
  1.  _W_: the spreading activation from the internal correctness
      criterion. This corresponds to WM in ACT-R and should
      correspond to _v_ in DDM.
  2.  _S_: The noise in declarative memory. This mimics the
      resistance to spreading activation.
  3.  _U_: The perceived utility of restarting the search process,
      defined in terms of RL expected value V. This determines
      the boundary parameter _a_.

### 2AFC Task Interface

The ACT-R device implements a minimal implementation of a 2AFC
task. Specifically, the task contains only one stimulus, and two
responses (``_left_'' and ``right'') are possible. By default, _left_
is assumed to be the correct response.   
