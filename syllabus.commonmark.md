Multidimensional Measurement Models Syllabus (PSQF 7375; Fall 2023)
================

*Note:* The [online
syllabus](https://jonathantemplin.github.io/MultidimensionalMeasurementModels2023/)
will always have the most current information.

## Course Information

|                      |                                                                            |
|----------------------|---------------------------------------------------------------------------:|
| Instructor:          |                                                           Jonathan Templin |
| email:               |                                                 jonathan-templin@uiowa.edu |
| Course website:      | <https://jonathantemplin.github.io/MultidimensionalMeasurementModels2023/> |
| Office:              |                                                     S210B Lindquist Center |
| Office Phone:        |                                                               319-335-6429 |
| Classroom:           |                                                     S210A Lindquist Center |
| Course Meeting Time: |                                                              W 12:30-15:20 |
| Course Office Hours: |                                            F 09:00-11:00 or by appointment |
|                      |                                                                            |

## Course Introduction

Multidimensional Measurement Models is an advanced PhD-level course with
a specialized focus on applications of measurement models with more than
one latent variable, primarily in the fields of education and the social
sciences. This comprehensive course is designed to provide an in-depth
exploration of multidimensional item response theory (MIRT), diagnostic
classification models (DCMs), and mixture models as essential tools for
understanding complex psychological and social phenomena.

As contemporary psychological measurement in the educational and social
sciences continues to grow in complexity, the need for sophisticated
measurement techniques has become increasingly apparent. Traditional
unidimensional measurement models often fall short in capturing the
multifaceted nature of human attributes and behaviors, hindering
accurate assessments and classifications. In response to these
challenges, multidimensional measurement models, including mixture
models, have emerged as powerful tools, enabling researchers and
practitioners to more effectively measure and understand intricate
constructs.

This course is designed for PhD-level students with a strong background
in psychometrics, statistical modeling, and quantitative research
methods. Participants should be familiar with basic unidimensional item
response theory and have experience working with statistical software
such as R.

## Course Objectives

This course will provide students with an understanding of
multidimensional measurement models, their theoretical underpinnings,
and their practical applications in educational and social science
contexts. By the end of the course, participants will be able to:

1.  **Understand Multidimensional Item Response Theory (MIRT):** Delve
    into the foundational principles of MIRT, examining how it extends
    traditional unidimensional models to account for the measurement of
    multiple latent dimensions. Understand the mathematical and
    statistical intricacies involved in fitting MIRT models to empirical
    data.

2.  **Analyze Multidimensional Data:** Gain proficiency in applying
    advanced statistical techniques to analyze multidimensional data.
    Explore methods for estimating latent trait scores, examining item
    characteristics, and assessing model fit for complex measurement
    structures.

3.  **Interpret Diagnostic Classification Models (DCMs):** Explore
    diagnostic classification models, a class of psychometric models
    that focus on classifying individuals into latent classes or
    profiles based on their response patterns. Understand how DCMs
    enhance diagnostic accuracy by accounting for multidimensionality
    and differential item functioning.

4.  **Examine Mixture Models:** Study mixture models as a class of
    multidimensional measurement models that account for heterogeneity
    within populations. Learn how mixture models can uncover distinct
    latent subgroups characterized by unique response patterns, shedding
    light on hidden structures within the data.

5.  **Apply Models in Education and Social Sciences:** Discover the
    practical applications of multidimensional measurement models,
    including mixture models, in various domains. Explore how these
    models can inform decision-making, educational planning, and social
    intervention strategies.

6.  **Critically Evaluate Research Literature:** Develop the skills to
    critically analyze and evaluate existing research studies that
    utilize multidimensional measurement models, including mixture
    models. Understand the advantages and limitations of these models in
    addressing complex research questions.

## Course Structure

The course is organized into modules that progressively build a solid
foundation in MIRT, DCMs, and mixture models. The modules will cover
theoretical concepts, mathematical formulations, computational
techniques, and practical implementation. Participants will engage in
hands-on exercises, data analysis projects, and discussions to reinforce
their understanding and application of these advanced measurement
models.

Each class meeting (170 minutes) will be divided into four sections:

1.  A lecture (approximately 80 minutes; from 12:30-13:50 each class
    meeting)
2.  A break (10 minutes; from 13:50-14:00 each class meeting)
3.  A discussion of the reading(s) from each week (approximately 20
    minutes; from 14:00-14:20 each class meeting)
4.  Coding/programming exercises (approximately 60 minutes; from
    14:20-15:20 each class meeting)

**Class will begin promptly at 12:30 each Wednesday.** Please arrive by
that time to limit distractions to those listening to lecture.

## Coursework

There will not be a hybrid option this semester; all students are
expected to attend class in person. If you are not feeling well, you can
watch the course videos when they are posted on YouTube, which should

### Course Assignments

A set of homework assignments will be administered throughout the
semester. These assignments will be designed to help you learn the
material and to provide you with an opportunity to practice estimation
of each model. The assignments will be graded on a 100-point scale. The
assignments will be due at 11:59pm on the date indicated on the course
website. Late assignments will not be accepted.

### Course Readings

The course will use a sample of book chapters and papers, with at least
one reading assigned each week. The readings will be available on ICON.
**You are expected to read the assigned readings before class and
participate in the class discussion of each reading.**

### Course Website/Technology

ICON **will** be used for grades, formative assessments, submission of
assignments, disseminating course readings, and course communications.

ICON **will not** be used for lecture materials. Instead, we will use
freely available commercial software for communication and dissemination
of course materials. Course lecture slides, lecture examples, video
files, assignments, and information are available on the website,
<https://jonathantemplin.github.io/MultidimensionalMeasurementModels2023/>.

All lectures will be archived on YouTube (my YouTube channel is
https://jonathantemplin.com/YouTube).

### Statistical Computing

The course will use the R statistical package with the R Studio
development suite along with a set of R packages (both Bayesian and
Non-Bayesian). As we will be investigating several estimation methods,
we will use a variety of R packages, Just Another Gibbs Sampler
[https://mcmc-jags.sourceforge.io/](JAGS), [https://mc-stan.org](Stan),
and [https://www.statmodel.com](Mplus). All packages have been built
into an Apptainer container that can be used on Argon, the University of
Iowa’s HPC cluster.

We will use Argon in our in-class coding exercises. To access Argon, you
must have an SSH client installed on your machine. Mac and Linux users
have SSH clients already installed, but Windows machines do not. If you
are using a Windows machine, you will need to install an SSH client. I
recommend [https://www.chiark.greenend.org.uk/\~sgtatham/putty/](PuTTY)
for Windows users or installing the free Windows Subsystem for Linux
(WSL) and using the SSH client in the WSL.

The University of Iowa enables access for many of these programs through
their research computing resources: R Studio Notebooks:
https://notebooks.hpc.uiowa.edu/ High Performance Computing:
https://hpc.uiowa.edu/

### Grading

Student evaluation will be made based three components: (1) homework
assignments (50% of course grade), (2) formative assessments (20% of
course grade), and (3) a one-on-one verbal final examination (30% of
course grade).

Mathematically, the grade percentage can be expressed as:

$$GP = .5\times HP + .2 \times FP + .3 \times F, $$

Where:

- GP is the Grade Percentage
- HP is the Homework Percentage
- FP is the Formative Assessment Percentage
- F is the Final Examination Percentage

### Homework Assignments

There will be a set of homework assignments (the exact number to be
determined). For each assignment, students will have a minimum of one
week to complete the assignment. Homework assignments will weighted
equally with respect to the 50% of the course grade accounted for by
homework. The lowest homework percentage will be dropped (rather than
allowing late homework).

Mathematically, the homework percentage can be expressed as:

$$HP = \left[\frac{\left( \sum_{h=1}^H P_h \right)-\min_h P_h} {H-1}\right],$$

where $P_h$ is the percent correct on homework $h$, with $H$ being the
total number of homework assignments.

In order to be able to provide the entire class with prompt feedback,
late homework assignments will not be accepted. However, extensions may
be granted as needed for extenuating circumstances (e.g., conferences,
family obligations) if requested at least three weeks in advance of the
due date. Additionally, late homework due to emergencies will be
accepted with documentation of the circumstances of the emergency.

All assignments must be completed in R, using R Markdown as a file
format, and submitted via ICON. Although students are encouraged to work
together on the concepts underlying homework, all answers must be from
student’s own work (writing and syntax) and not be copied or paraphrased
from anyone else’s answers. Grammar and writing will be assessed by each
homework and will factor into the homework grade.

### Formative Assessments

Each week, there will be a short formative assessment in ICON. The
purpose of the formative assessment is to help students obtain a picture
of their understanding of course materials. All genuine attempts (i.e.,
not haphazard answers) at completion will receive full credit. All
formative assessments will be weighted equally with respect to the 20%
of the course grade.

### Verbal Final Examination

As we live in the era of large language models, rather than a written
project, students will be required to complete a verbal final
examination. The verbal final examination will be a one-on-one meeting
with the instructor. The verbal final examination will account for 30%
of the course grade.

The contents of the final will be the entirety of the course materials,
with an emphasis on the readings and the homework assignments. The
verbal final examination will be held from 12pm-4pm on Wednesday,
December 13th. A sign-up sheet for an examination time blocks will be
posted on ICON.

## Course Grading System

| Point Total   | Letter Grade |
|---------------|:-------------|
| 100 and Above | A+           |
| 99-93         | A            |
| 92-90         | A-           |
| 89-87         | B+           |
| 86-83         | B            |
| 82-80         | B-           |
| 79-77         | C+           |
| 76-73         | C            |
| 72-70         | C-           |
| 69-60         | D            |
| Below 60      | F            |
|               |              |

## University of Iowa Course Policies and Resources for Students

Please see https://provost.uiowa.edu/student-course-policies for
additional policies and resources.
