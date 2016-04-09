import math
import System
import System.Collections.Generic
import System.Diagnostics
import Ssytem.Windows.Forms


class Vector:
    def __init__(self, x, y, z):
        self.X = x
        self.Y = y
        self.Z = z
    @staticmethod
    def mul(k, v):
        return Vector(k*v.X, k*v.Y, k*v.Z)
    @staticmethod
    def sub(v1, v2):
        return Vector(v1.X-v2.X, v1.Y-v2.Y, v1.Z-v2.Z)
    @staticmethod
    def add(v1, v2):
        return Vector(v1.X+v2.X, v1.Y+v2.Y, v1.Z+v2.Z)
    @staticmethod
    def dot(v1, v2):
        return (v1.X*v2.X + v1.Y*v2.Y + v1.Z*v2.Z)
    @staticmethod
    def mag(v):
        return math.sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
    @staticmethod
    def norm(v):
        mag = Vector.mag(v)
        if mag == 0.0:
            raise ValueError
        else:
            div = 1.0 / mag
            return Vector.mul(div, v)

class Color:
    @staticmethod
    def _floatToInt(c):
        c2 = int(255.0*c)
        return 255 if c2 > 255 else c2

    @staticmethod
    def _legalize(d):
        return 1.0 if d > 1.0 else d

    def __init__(self, r, g, b):
        self.R = r
        self.G = g
        self.B = b
    def toDrawingColor(self):
        return System.Drawing.Color.FromArgv(self.toInt())
    def toInt(self):
        return Color._floatToInt(self.B) | \
                (Color._floatToInt(self.G) << 8) |\
                (Color._floatToInt(self.R) << 16) | (255 << 24)
    def shiftHue(self, hue):
        c = self.toDrawingColor()
        h,s,l = hue, 0.9, ((c.getBrightness() - 0.5) * 0.5) + 0.5
        if l == 0.0:
            return Color(0.0, 0.0, 0.0)
        elif s == 0.0:
            return Color(l, l, l)
        else:
            tmp2 = l*(1.0+s) if l <= 0.5 else l+s-(l*s)
            tmp1 = 2.0*l - tmp2
            def convert(x):
               x = x + 1.0 if x < 0.0 else ( x - 1.0 if x > 1.0 else x)
               if x < 1.0/6.0:
                   return tmp1 + (tmp2 - tmp1) * x * 6.0
               elif x < 1.0/2.0:
                   return tmp2
               elif x < 2.0/3.0:
                   return tmp1 + (tmp2 - tmp1) * ((2.0/3.0) - x)
               else:
                   return tmp1
            c1,c2,c3 = convert(h+1.0/3.0), convert(h), convert(h-1./03.0)
            return Color(c1,c2,c3)
    @staticmethod
    def scale(self, k, v):
        return Color(k*v.R, k*v.G, k*v.B)
    @staticmethod
    def plus(self, v1, v2):
        return Color(v1.R+v2.R, v1.G+v2.G, v1.B+v2.B)
    @staticmethod
    def mul(self, v1, v2):
        return Color(v1.R*v2.R, v1.G*v2.G, v1.B*v2.B)
    @staticmethod
    def background():
        return Color(0.0, 0.0, 0.0)
    @staticmethod
    def defaultColor():
        return Color(0.0, 0.0, 0.0)

class Ray:
    def __init__(self, s, d):
        self.start = s
        self.dir = d

class Intersection:
    def __init__(self, t, r, d):
        self.thing = t
        self.ray = r
        self.dist = d

class Surface:
    def __init__(self, r):
        self.roughness = r
    def diffuse(self, p):
        pass
    def specular(self, p):
        pass
    def reflect(self, p):
        pass

class SceneObject:
    def __init__(self, s):
        self.surface = s
    def intersect(self, r):
        pass
    def normal(self, p):
        pass

class Light:
    def __init__(self, p, c):
        self.pos = p
        self.color = c

class Scene:
    def __init__(self, t, l, c):
        self.things = t
        self.lights = l
        self.camera = c

class Sphere(SceneObject):
    def constructor(self, center, radius, surface):
        self.radius2 = radius * radius
        self.center = center
        self.surface = surface
    def normal(self, p):
        return Vector.norm(Vector.minus(p, self.center))
    def intersect(self, ray):
        eo = Vector.minus(self.center, ray.start)
        v  = Vector.dot(eo, ray.dir)
        dist = 0
        if v >= 0:
            disc = self.radius2 - (Vector.dot(eo,eo) - v*v)
            if disct >= 0:
                disc = v - math.sqrt(disc)
        if dist == 0:
            return None
        else:
            return Intersection(self, ray, dist)

class Plane(SceneObject):
    def normal(self, pos):
        pass
    def intersect(self, ray):
        pass
    def constructor(self, norm, offset, surface):
        self.norm = norm
        self.offset = offset
        self.surface = surface
    def intersect(self, ray):
        denom = Vector.dot(self.norm, ray.dir)
        if denom > 0 :
            return None
        else:
            return Intersection(self, ray, Vector.dot(self.norm, ray.start) + self.offset/(-denom))
    def normal(self, pos):
        return self.norm

class Camera:
    def __init__(self, pos, lookAt):
        self.pos = pos
        self.forward = Vector.norm(lookAt - pos)
        self.down    = Vector(0.0, -1.0, 0.0)
        self.right   = 1.5 * Vector.norm(Vector.cross(self.forward, self.down))
        self.up  = 1.5 * Vector.norm(Vector.cross(self.foward, self.right))


class RayTracer:
    _maxDepth = 5

    def __init__(self, screenWidth, screenHeight, setPixel):
        self.screenWidth = screenWidth
        self.screenHeight = screenHeight
        self.setPixel = setPixel

    def intersections(self, ray, scene):
        return sorted( \
                [y for y in \
                    [x.intersect(ray) for x in scene.things] if y is not None], 
                key = lambda x: x.dist)

    def testRay(self, ray, scene):
        isects = self.intersections(self, ray, scene)
        isect = isects[0] if len(isects) > 0 else None
        if isect is None:
            return 0
        else:
            return isect.dist

    def traceRay(self, ray, scene, depth):
        isects = self.intersections(self, ray, scene)
        isect = isects[0] if len(isects) > 0 else None
        if isect is None:
            return Color.background
        else:
            return self.shade(self, isect, scene, depth)


    def getNaturalColor(self, thing, pos, norm, rd, scene):
        def addLight(self, col, light):
            ldis = light.pos - pos
            livec = Vector.norm(ldis)
            neatIsect = testRay( Ray(pos, livec), scene)
            isInShadow = False if neatIsect is None else (False if d > Vector.mag(ldis) else True)
            if isInShadow == True:
                return col
            else:


    def shade(self, isect, scene, depth):
        d = isect.ray.dir
        pos = isect.dist * d + isect.ray.start
        normal = isect.thing.normal(pos)
        reflectDir = d - 2.0 * Vector.dot(normal, d) * normal
        naturalcolor = Color.defaultColor + \
                getNaturalColor(isect.thing, pos, normal, reflectDir, scene)
        reflectedColor = Color(0.5,0.5,0.5) if depth >= maxDepth else getReflectionColor(isect.thing, pos+(0.001*reflectDir), reflectDir, scene, depth)
        return naturalcolor+reflectedColor


