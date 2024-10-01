from PIL import ImageGrab
import sys

def main(argv):
    rel_path = argv[1]
    full_path = argv[2]
    im = ImageGrab.grabclipboard()
    if im != None:
        im.save(full_path, 'PNG')
        print(f"[[file:{rel_path}]]")
    # print(f"{argv[0]} {argv[1]}\n")


if __name__ == "__main__":
    main(sys.argv)
