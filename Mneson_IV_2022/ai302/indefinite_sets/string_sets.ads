with Indefinite_Sets;
pragma Elaborate_All (Indefinite_Sets);

package String_Sets is
   new Indefinite_Sets (String);

pragma Preelaborate (String_Sets);
--ok because null_cursor isn't a constant anymore



