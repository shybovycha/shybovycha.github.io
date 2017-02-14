import scss from 'rollup-plugin-scss'
import uglify from 'rollup-plugin-uglify'

export default {
  entry: 'main.js',
  dest: 'public/bundle.js',
  format: 'iife',
  plugins: [
    scss({
      output: 'public/bundle.css'
    }),

    // uglify()
  ]
}
