with AI302.Containers.Vectors;
pragma Elaborate_All (AI302.Containers.Vectors);

package Character_Vectors is
   new AI302.Containers.Vectors (Natural, Character);

pragma Preelaborate (Character_Vectors);


