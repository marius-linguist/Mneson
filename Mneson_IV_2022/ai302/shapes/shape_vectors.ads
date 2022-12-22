with AI302.Containers.Vectors;
pragma Elaborate_All (AI302.Containers.Vectors);

with Shapes;  use Shapes;

package Shape_Vectors is
   new AI302.Containers.Vectors (Positive, Shape_Class_Access);

--pragma Preelaborate (Shape_Vectors);
