import argparse, random, sys

class DiskScheduler:
    def __init__(self, initial_position, access_sequence=None):
        self.initial_position = initial_position
        self.access_sequence = access_sequence
    
    # Function to generate a random sequence if one is not provided
    def random_sequence(self):
        sequence = []
        for _ in range(100):
            cylinder = random.randint(0, 4999)
            sequence.append(cylinder)
        return sequence

    # Function to go through the provided file to extract the given sequence
    def create_file_sequence(self, provided_file):
        sequence = []
        with open(provided_file, 'r') as file:
            for line in file:
                sequence.append(int(line.strip()))
        return sequence

    # Function to simulate First Come First Serve Algorithm
    def fcfs(self):
        distance = abs(self.access_sequence[0] - self.initial_position)
        for i in range(1, len(self.access_sequence)):
            distance += abs(self.access_sequence[i] - self.access_sequence[i-1])
        return distance

    # Function to simulate Shortest Seek Time First Algorithm
    def sstf(self):
        sorted_sequence = []

        # Calculates absolute difference between all elements (starting from initial), sorting as given
        sorted_sequence = sorted(self.access_sequence, key=lambda x: abs(x - self.initial_position))
        distance = abs(sorted_sequence[0] - self.initial_position)
        for i in range(1, len(sorted_sequence)):
            distance += abs(sorted_sequence[i] - sorted_sequence[i-1])
        return distance

    # Function to simulate SCAN Algorithm
    def scan(self):
        sorted_sequence = []

        # List of elements to the right of the initial position
        right_elements = [x for x in self.access_sequence if x >= self.initial_position]
        right_elements.sort(key=lambda x: x - self.initial_position)
        right_elements.append(4999)

        # List of elements to the left of the initial position
        left_elements = [x for x in self.access_sequence if x < self.initial_position]
        left_elements.sort(key=lambda x: 4999 + x - self.initial_position, reverse=True)

        sorted_sequence.extend(right_elements)
        sorted_sequence.extend(left_elements)

        distance = abs(sorted_sequence[0] - self.initial_position)
        for i in range(1, len(sorted_sequence)):
            distance += abs(sorted_sequence[i] - sorted_sequence[i-1])
        return distance

    # Function to simulate C-SCAN Algorithm
    def cscan(self):
        sorted_sequence = []

        # List of elements to the right of the initial position
        right_elements = [x for x in self.access_sequence if x >= self.initial_position]
        right_elements.sort(key=lambda x: x - self.initial_position)
        right_elements.append(4999)

        # List of elements to the left of the initial position
        left_elements = [x for x in self.access_sequence if x < self.initial_position]
        left_elements.sort(key=lambda x: x - self.initial_position)

        sorted_sequence.extend(right_elements)
        sorted_sequence.extend(left_elements)

        distance = abs(sorted_sequence[0] - self.initial_position)
        for i in range(1, len(sorted_sequence)):
            distance += abs(sorted_sequence[i] - sorted_sequence[i-1])
        return distance


    # Function to simulate LOOK Algorithm
    def look(self):
        sorted_sequence = []

        # List of elements to the right of the initial position
        right_elements = [x for x in self.access_sequence if x >= self.initial_position]
        right_elements.sort(key=lambda x: x - self.initial_position)

        # List of elements to the left of the initial position
        left_elements = [x for x in self.access_sequence if x < self.initial_position]
        left_elements.sort(key=lambda x: 4999 + x - self.initial_position, reverse=True)

        sorted_sequence.extend(right_elements)
        sorted_sequence.extend(left_elements)

        distance = abs(sorted_sequence[0] - self.initial_position)
        for i in range(1, len(sorted_sequence)):
            distance += abs(sorted_sequence[i] - sorted_sequence[i-1])
        return distance

    # Function to simulate C-LOOK Algorithm
    def clook(self):
        sorted_sequence = []

        # List of elements to the right of the initial position
        right_elements = [x for x in self.access_sequence if x >= self.initial_position]
        right_elements.sort(key=lambda x: x - self.initial_position)

        # List of elements to the left of the initial position
        left_elements = [x for x in self.access_sequence if x < self.initial_position]
        left_elements.sort(key=lambda x: x - self.initial_position)

        sorted_sequence.extend(right_elements)
        sorted_sequence.extend(left_elements)

        distance = abs(sorted_sequence[0] - self.initial_position)
        for i in range(1, len(sorted_sequence)):
            distance += abs(sorted_sequence[i] - sorted_sequence[i-1])
        return distance

    

def main(): 
    parser = argparse.ArgumentParser(
        description="Disk Scheduling Simulator",
        usage="%(prog)s <INITIAL_POSITION> [<ACCESS_SEQUENCE_FILE>]"
    )
    parser.add_argument('initial_position', type=int, help='Initial disk and travel position [-4999/4999]')
    parser.add_argument('access_sequence_file', nargs='?', help='File representing disk cylinders to be seeked')
    args = parser.parse_args()
    
    disk = DiskScheduler(initial_position = args.initial_position)

    if(args.access_sequence_file == None):
        disk.access_sequence = disk.random_sequence()     # if no file is provided by the user, randomize it
    
    else:
        disk.access_sequence = disk.create_file_sequence(args.access_sequence_file)

    print("FCFS ", disk.fcfs())
    print("SSTF ", disk.sstf())
    print("SCAN ", disk.scan())
    print("C-SCAN ", disk.cscan())
    print("LOOK ", disk.look())
    print("C-LOOK ", disk.clook())
    
  
if __name__=="__main__": 
    main() 