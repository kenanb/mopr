## Dependency File Generation

# Documentation source: Automatic Prerequisites
# Dependency files are effectively makefiles with a single rule.
#
# Dependency  file itself  should also  be updated  when the  files in
# dependency list are  modified.  So we are  declaring dependency file
# itself as an  intermediate target. Note that  this approach diverges
# from the  documented pattern, which  would make both the  object and
# dependency files depend on the same sources. However, I decided this
# approach makes the dependency chain more obvious. The downside is, I
# am  having to  rebuild the  cpp source  path from  fragments of  the
# target object  path. But I prefer  prepending a dot to  filenames of
# object and dependency files.  So I need to rebuilt the path anyway.
#
# Example x.cpp.d that represents dependencies of x.cpp:
#   src/.x.cpp.d: x.cpp x.h a.h b.h

.%.cpp.d: %.cpp
	$(call ECHO_RULE)
	$(CXX) -MM -MT "$@" $(CXXFLAGS) $< > $@

# Object files.
.%.cpp.o: .%.cpp.d
	$(call ECHO_RULE)
	$(CXX) -c $(CXXFLAGS) -o $@ $(*D)/$(*F).cpp

.%.c.d: %.c
	$(call ECHO_RULE)
	$(CC) -MM -MT "$@" $(CFLAGS) $< > $@

# Object files.
.%.c.o: .%.c.d
	$(call ECHO_RULE)
	$(CC) -c $(CFLAGS) -o $@ $(*D)/$(*F).c

.%.asd.d: %.asd
	$(call ECHO_RULE)
	echo -n > $@
	for f in `find $(*D) -type f -name '*.h'`; do $(CC) -MM -MT "$@" $(CFLAGS) $$f >> $@; done
	echo "$@ :" `find $(*D) -type f -name '*.asd' -o -name '*.lisp' -o -name '*.spec' | tr "\n" " "` >> $@
