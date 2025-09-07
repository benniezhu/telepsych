mental_icd9_codes <- c(
  "295", "296", "297", "298", "299", "300", "301", "302",
  "305", "306", "307", "308", "309", "310", "311", "312", "313", "314"
)
mental_icd10_codes <- c(
  paste0("F", 20:29),  # Schizophrenia, schizotypal, delusional, and other non-mood psychotic disorders
  paste0("F", 30:39),  # Mood [affective] disorders
  paste0("F", 40:48),  # Anxiety, dissociative, stress-related, somatoform and other nonpsychotic mental disorders
  paste0("F", 50:59),  # Behavioral syndromes associated with physiological disturbances and physical factors
  paste0("F", 60:69),  # Disorders of adult personality and behavior
  paste0("F", 70:79),  # Intellectual disabilities
  paste0("F", 80:89),  # Pervasive and specific developmental disorders
  paste0("F", 90:98),  # Behavioral and emotional disorders with onset usually occurring in childhood and adolescence
  "F99"               # Unspecified mental disorder
)

# 
# 
# 
# F20-F29  Schizophrenia, schizotypal, delusional, and other non-mood psychotic disorders
# F30-F39  Mood [affective] disorders
# F40-F48  Anxiety, dissociative, stress-related, somatoform and other nonpsychotic mental disorders
# F50-F59  Behavioral syndromes associated with physiological disturbances and physical factors
# F60-F69  Disorders of adult personality and behavior
# F70-F79  Intellectual disabilities
# 
# F80-F89  Pervasive and specific developmental disorders
# F90-F98  Behavioral and emotional disorders with onset usually occurring in childhood and adolescence
# F99-F99  Unspecified mental disorder
# 
# icd9
# 295 Schizophrenic disorders
# 296 Episodic mood disorders
# 297 Delusional disorders
# 298 Other nonorganic psychoses
# 299 Pervasive developmental disorders
# 300 Anxiety, dissociative and somatoform disorders
# 301 Personality disorders
# 302 Sexual and gender identity disorders
# 
# 
# 
# 305 Nondependent abuse of drugs
# 306 Physiological malfunction arising from mental factors
# 307 Special symptoms or syndromes not elsewhere classified
# 308 Acute reaction to stress
# 309 Adjustment reaction
# 310 Specific nonpsychotic mental disorders due to brain damage
# 311 Depressive disorder, not elsewhere classified
# 312 Disturbance of conduct not elsewhere classified
# 313 Disturbance of emotions specific to childhood and adolescence
# 314 Hyperkinetic syndrome of childhood