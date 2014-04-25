
'use scrict';

// Basic template description.
exports.description = 'Scala project using sbt for building'

// Template-specific notes to be displayed before question prompts.
exports.notes = '';

// Template-specific notes to be displayed after question prompts.
exports.after = '';

// Any existing file or directory matching this wildcard will cause a warning.
exports.warnOn = '*';

// The actual init template.
exports.template = function(grunt, init, done) {

  init.process({}, [
    // Prompt for these values.
    //init.prompt('name'),
    //init.prompt('npm_test', 'grunt nodeunit')
  ], function(err, props) {
    props.keywords = [];
    /*
    props.devDependencies = {
      'grunt-contrib-concat': '~0.3.0',
    };
    */

    // Files to copy (and process).
    var files = init.filesToCopy(props);

    // Add properly-named license files.
    //init.addLicenseFiles(files, props.licenses);

    // Actually copy (and process) files.
    init.copyAndProcess(files, props);

    // Create the scala directories
    grunt.file.mkdir('src/main/java');
    grunt.file.mkdir('src/main/scala');
    grunt.file.mkdir('src/main/resources');
    grunt.file.mkdir('src/test/java');
    grunt.file.mkdir('src/test/scala');
    grunt.file.mkdir('src/test/resources');
    grunt.file.mkdir('lib');

    // All done!
    done();
  });

};

