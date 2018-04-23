/**
 * Wavelength to RGB color
 * Algorithm from: http://www.efg2.com/Lab/ScienceAndEngineering/Spectra.htm
 * @author Beren Oguz
 * @copyright 2015, Beren Oguz
 * @license This code is licensed with BSD-2 clause License
 *          Please see: http://opensource.org/licenses/BSD-2-Clause
 */

#include <cmath> // for std::trunc, std::round, std::pow

typedef double Real; // Any floating-point type would work such as float or long double
typedef int Integer;
typedef unsigned int Byte; // Because in RGB, each of the properties can be at most 255

struct RGB // One may create a better RGB color class
           // For simplicity I'll leave it a POD struct
{
	Byte red;
	Byte green;
	Byte blue;
	RGB& operator = (const RGB& other)
	{
		this->red = other.red;
		this->green=other.green;
		this->blue=other.blue;

		return *this;
	}
};

/**
 * @function Convert wavelength in nanometers to RGB color
 * @param wavelength in nanometers
 * @return the color infered by human eyes, in RGB format
 */
RGB to_rgb(Real wavelength)
{
    auto adjust = [](Real color, Real factor)->Integer
    {
        constexpr Real gamma = 0.8;
        constexpr Integer intensity_max = 255;

        if(color == 0)
        {
            return 0;
        }
        else
        {
            return (std::round(intensity_max * std::pow(color*factor,gamma)));
        }
    };

    Real r;
    Real g;
    Real b;
    Real factor;

    switch((Integer)std::trunc(wavelength))
    {
        case 380 ... 439:
            r = -(wavelength - 440) / (440 - 380);
            g = 0;
            b = 1;
            break;
        case 440 ... 489:
            r = 0;
            g = (wavelength - 440) / (490 - 440);
            b = 1;
            break;
        case 490 ... 509:
            r = 0;
            g = 1;
            b = -(wavelength - 510) / (510 - 490);
            break;
        case 510 ... 579:
            r = (wavelength - 510) / (580 - 510);
            g = 1;
            b = 0;
            break;
        case 580 ... 644:
            r = 1;
            g = -(wavelength - 645) / (645 - 580);
            b = 0;
            break;
        case 645 ... 780:
            r = 1;
            g = 0;
            b = 0;
            break;
        default:
            r = 0;
            g = 0;
            b = 0;
            break;
    }

    switch((Integer)std::trunc(wavelength))
    {
        case 380 ... 419:
            factor = 0.3 + 0.7 * (wavelength - 380) / (420 - 380);
            break;
        case 420 ... 700:
            factor = 1;
            break;
        case 701 ... 780:
            factor = 0.3 + 0.7 * (780 - wavelength) / (780 - 700);
            break;
        default:
            factor = 0;
            break;
    }

    RGB rgb_color; // The color to be returned

    rgb_color.red = adjust(r, factor);
    rgb_color.green = adjust(g, factor);
    rgb_color.blue = adjust(b, factor);

    return rgb_color;
}