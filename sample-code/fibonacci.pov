// Fibonacci (POV-Ray)
#version 3.7;
global_settings { assumed_gamma 1.0 }

camera {
  location <0, 2, -6>
  look_at  <0, 1,  0>
}

light_source { <2, 4, -3> color rgb <1, 1, 1> }

sphere {
  <0, 1, 0>, 1
  texture {
    pigment { color rgb <0.2, 0.4, 0.9> }
    finish  { phong 0.6 }
  }
}

// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
// END OF FIBONACCI SAMPLE
