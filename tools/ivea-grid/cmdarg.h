#ifndef __CMDARG_H__
#define __CMDARG_H__

#include <getopt.h>
#include <string.h>

typedef struct {
  // IVEA3H or ISEA3H
  const char* type;
  int refinement;
  int subzones;
  bool adjacency;
  bool tesselate;
  int level;
} CmdArgs;

static void print_usage(const char *progname) {
    fprintf(stderr,
            "Usage: %s [--type <str>] [--refinement <int>] [--subzones <int>] [--no-adjacency] --level <int>\n"
            "  --type         grid type — IVEA3H (default) or ISEA3H\n"
            "  --refinement   zone geometry refinement level (default: 5)\n"
            "  --subzones     subzone depth, 0 to omit subzones (default: 2)\n"
            "  --tesselate    generate tesselated triangles (default: no)\n"
            "  --no-adjacency suppress adjacent zone information\n"
            "  --level        grid level\n",
            progname);
}

CmdArgs parse_args(int argc, char *argv[]) {
  CmdArgs args = {
    .type       = "IVEA3H",
    .refinement = 5,
    .subzones   = 2,
    .adjacency = true,
    .tesselate = false,
    .level = -1
  };

  /* Define the long options */
  struct option long_options[] = {
    { "type",         required_argument, 0, 't' },
    { "refinement",   required_argument, 0, 'r' },
    { "subzones",     required_argument, 0, 's' },
    { "level",        required_argument, 0, 'l' },
    { "no-adjacency", no_argument,       0, 'a' },
    { "tesselate",    no_argument,       0, 'x' },
    { 0, 0, 0, 0 }
  };

  int opt, opt_index = 0;
  while ((opt = getopt_long(argc, argv, "t:r:s:", long_options, &opt_index)) != -1) {
    switch (opt) {
      case 't':
        args.type = optarg;
        break;
      case 'r':
        args.refinement = atoi(optarg);
        break;
      case 's':
        args.subzones = atoi(optarg);
        break;
      case 'a':
        args.adjacency = false;
        break;
      case 'x':
        args.tesselate = true;
        break;
      case 'l':
        args.level = atoi(optarg);
        break;
      case '?':  /* unknown option or missing argument */
      default:
        print_usage(argv[0]);
        exit(EXIT_FAILURE);
    }
  }

  /* No additional arguments are permitted */
  if (optind < argc) {
    fprintf(stderr, "Unexpected argument: %s\n", argv[optind]);
    print_usage(argv[0]);
    exit(EXIT_FAILURE);
  }

  // validate the arguments
  if (!(args.level >= 0 && args.level <= 12)) {
    fprintf(stderr, "--level must be an integer between 0 and 12\n");
    exit(EXIT_FAILURE);
  }

  if (strcmp(args.type, "IVEA3H") != 0 && strcmp(args.type, "ISEA3H") != 0) {
    fprintf(stderr, "--type must be either IVEA3H or ISEA3H\n");
    exit(EXIT_FAILURE);
  }

  if (!(args.refinement >= 0 || args.refinement <= 100)) {
    fprintf(stderr, "--refinement must be an integer between 0 and 100\n");
    exit(EXIT_FAILURE);
  }

  if (!(args.subzones >= 0 || args.subzones <= 5)) {
    fprintf(stderr, "--subzones must be an integer between 0 and 5\n");
    exit(EXIT_FAILURE);
  }

  return args;
}


#endif
